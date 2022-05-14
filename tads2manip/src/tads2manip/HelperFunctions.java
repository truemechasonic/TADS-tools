package tads2manip;

import java.nio.ByteBuffer;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.Scanner;

public class HelperFunctions {
    public static int EXPECT_NOTHING = 0;
    public static int EXPECT_PROP = 1;
    
    public static short toShort(String strPlusChars, ArrayList<String> bnames, ArrayList<ArrayList<Integer>> bvals) {
        String string;
        if(strPlusChars.charAt(0) == ';') {
            string = strPlusChars.substring(1, strPlusChars.length() - 1).trim();
        } else {
            string = strPlusChars;
        }
        String[] split = string.split(" ");
        short value = 0;
        if(split.length <= 2) {
            for(int index = split.length - 1; index >= 0; index--) {
                String str = split[index];
                value *= 256;
                if(str.startsWith("0x")) {
                    value += Short.parseShort(str.substring(2), 16);
                } else if(str.startsWith("@")) {
                    String temp = str.substring(1);
                    boolean found = false;
                    for(int y = bnames.size()-1; y >= 0; y--) {
                        if(bnames.get(y).equals(temp)) {
                            for(int z = bvals.get(y).size() - 1; z >= 0; z--) {
                                int byt = bvals.get(y).get(z);
                                value += byt;
                                if(z != 0) {
                                    value *= 256;
                                }
                            }
                            found = true;
                            break;
                        }
                    }
                    if(!found) {
                        System.out.println("ERROR: byte var not found (" + temp + ").");
                    }
                } else {
                    value += Short.parseShort(str);
                }
            }
        } else {
            System.err.println("ERROR: wtf?");
        }
        return value;
    }
    
    //returns numbytes
    public static int aobToByteBuffer(String aob, String separatorSearch, ByteBuffer bb, ArrayList<String> bnames, ArrayList<ArrayList<Integer>> bvals, int expect) {
        Map<String, Integer> labels = new HashMap<String, Integer>();
        ArrayList<String> jumpLabels = new ArrayList<String>();
        ArrayList<Integer> jumpLocs = new ArrayList<Integer>();
        
        String dquoteSearch = "\"[^\"]*\"";
        
        int len = 0;
        Scanner scanner = new Scanner(aob);
        while(scanner.hasNext()) {
            String next = scanner.next();
            if(next.equals("|")) {
                continue;
            }
                
            if(next.charAt(0) == '@' && bnames != null) {
                String temp = next.substring(1);
                boolean found = false;
                for(int y = bnames.size()-1; y >= 0; y--) {
                    if(bnames.get(y).equals(temp)) {
                        for(int z = 0; z < bvals.get(y).size(); z++) {
                            int byt = bvals.get(y).get(z);
                            byte b = (byte)byt;
                            if(byt > 127 && b >= 0) {
                                b -= 128;
                            }
                            
                            bb.put(b);
                            len++;
                        }
                        found = true;
                        break;
                    }
                }
                if(!found) {
                    System.out.println("ERROR: byte var not found (" + temp + ").");
                }
            } else if(next.charAt(0) == '*') {
                labels.put(next.substring(1).toLowerCase(), len);
            } else if(next.charAt(0) == '&') {
                jumpLabels.add(next.substring(1).toLowerCase());
                jumpLocs.add(len);
                bb.putShort((short)0);
                len += 2;
            } else if(next.equals("say")) {
                String str = scanner.findInLine(dquoteSearch);
                str = str.substring(1, str.length() - 1);
                bb.put((byte)0x1d);
                bb.putShort((short)(str.length() + 2));
                for(int x = 0; x < str.length(); x++) {
                    bb.put((byte)(str.charAt(x) & 0xff));
                }
                len += 3 + str.length();
            } else {
                int byt = Integer.valueOf(next, 16);
                byte b = (byte)byt;
                if(byt > 127 && b >= 0) {
                    b -= 128;
                }
                bb.put(b);
                len++;
            }
        }
        
        for(int x = 0; x < jumpLabels.size(); x++) {
            int offset = jumpLocs.get(x).intValue();
            int target = labels.get(jumpLabels.get(x)).intValue();
            
            short relative = (short) (target - offset);
            ByteBuffer temp = ByteBuffer.wrap(bb.array());
            temp.order(bb.order());
            temp.position(offset);
            temp.putShort(relative);
        }
        
        if(expect == EXPECT_PROP) {
            int psize = len - 6;
            ByteBuffer temp = ByteBuffer.wrap(bb.array());
            temp.order(bb.order());
            temp.position(3);
            int temps = temp.getShort();
            if(temps < psize) {
                temp.position(3);
                temp.putShort((short)psize);
            }
        }
        
        return len;
    }
}
