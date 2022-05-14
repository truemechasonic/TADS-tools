package tads2manip;

import static tads2manip.HelperFunctions.*;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.URL;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.util.ArrayList;
import java.util.Scanner;
import javax.swing.JButton;
import javax.swing.JTextArea;

//This file applies one of my 'patch' files to a tads2 game.

class TADS2patcherHelper implements Runnable {
    public static ArrayList<Obj> objs = new ArrayList<Obj>();
    public static ArrayList<Vocab> voc = null;
    byte[] header = new byte[48];
    
    public String in;
    public String out;
    public String pfile = null;
    public InputStream pfileStream = null;
    public String pfileContents = null;
    public JTextArea log = new JTextArea();
    public JButton btn = null;
    public boolean verbose = true;
    boolean patchFailed = false;
    
    int lastProp = 0;

    public static void main(String[] args) throws IOException {
        TADS2patcherHelper p = new TADS2patcherHelper();
        p.in = args[0];
        p.out = args[1];
        p.pfile = args[2];
        p.patch();
    }

    @Override
    public void run() {
        try {
            patch();
        } catch (IOException ex) {
            System.err.println(ex.toString());
            if(log != null) {
                log.append(ex.toString());
            }
        }
    }

    void patch() throws FileNotFoundException, IOException {
        patchFailed = false;
        log.setText("");
        log.append("Reading game file...\n");
        
        FileInputStream is = new FileInputStream(in);
        
        ArrayList<Resource> resources = new ArrayList<Resource>();
        CryptSettings crypt = new CryptSettings();

        is.read(header, 0, 48);
        if((header[20] & 0x8)  > 0) {
            crypt.encrypted = true;
        }

        String tempString = "";
        for(int x = 0; x < 9; x++) {
            tempString += (char)header[x];
        }
        if(!tempString.equals("TADS2 bin")) {
            System.err.println("ERROR: NOT A TADS2 GAME FILE!");
            if(log != null) {
                log.append("ERROR: NOT A TADS2 GAME FILE!\n");
            }
            if(btn != null) {
                btn.setEnabled(true);
            }
            return;
        }

        while(true) {
            int bite = is.read();
            StringBuilder builder = new StringBuilder();
            for(int x = 0; x < bite; x++) {
                builder.append((char)is.read());
            }
            String name = builder.toString();

            if(name.equals("$EOF")) {
                break;
            }

            int b1 = is.read();
            int b2 = is.read();
            int b3 = is.read();
            int b4 = is.read();
            int nextRsc = b4 * (256*256*256) + b3 * (256*256) + b2 * 256 + b1;
            int size = nextRsc - (int)is.getChannel().position();

            byte[] data = new byte[size];
            is.read(data, 0, size);

            ByteBuffer bb = ByteBuffer.wrap(data);
            bb.order(ByteOrder.LITTLE_ENDIAN);
            Resource rec = new Resource(name, bb, size, crypt);
            resources.add(rec);
            if(rec.voc != null) {
                voc = rec.voc;
            }
        }
        is.close();

        fixAddresses(resources);
        
        log.append("Applying patch...\n");
        
        try {
            parsePatch(pfile, pfileStream, pfileContents, log);
        } catch(Exception e) {
            System.err.println(e.toString());
            patchFailed = true;
        }
        
        if(!patchFailed) {
            fixAddresses(resources);

            log.append("Writing to disk...\n");

            FileOutputStream os = new FileOutputStream(out, false);
            os.write(header, 0, header.length);
            for(int x = 0; x < resources.size(); x++) {
                int size = resources.get(x).getSize();
                byte[] bytes = new byte[size];

                ByteBuffer bb = ByteBuffer.wrap(bytes);
                bb.order(ByteOrder.LITTLE_ENDIAN);
                resources.get(x).write(bb, (int)os.getChannel().position(), (int)(size+os.getChannel().position()), crypt);

                os.write(bytes, 0, size);
            }

            byte[] ending = new byte[9];
            ending[0] = 4;
            ending[1] = '$';
            ending[2] = 'E';
            ending[3] = 'O';
            ending[4] = 'F';
            ending[5] = 0;
            ending[6] = 0;
            ending[7] = 0;
            ending[8] = 0;
            os.write(ending, 0, 9);
            os.close();

            System.out.println("Patch complete.");
            log.append("Patch complete.\n");
        } else {
            System.out.println("ERROR: Patch failed.");
            log.append("ERROR: Patch failed.\n");
        }

        if(btn != null) {
            btn.setEnabled(true);
        }
    }

    private void parsePatch(String patchpath, InputStream patchStream, String patchContents, JTextArea log) throws FileNotFoundException {
        Scanner patch;
        if(patchStream != null) {
            patch = new Scanner(new BufferedReader(new InputStreamReader(patchStream)));
        } else if(patchpath != null) {
            patch = new Scanner(new BufferedReader(new FileReader(patchpath)));
        } else {
            patch = new Scanner(patchContents);
        }
        
        ArrayList<String> varNames = new ArrayList<String>();
        ArrayList<Boolean> varVals = new ArrayList<Boolean>();
        ArrayList<String> bNames = new ArrayList<String>();
        ArrayList<ArrayList<Integer>> bVals = new ArrayList<ArrayList<Integer>>();

        int totalChanges = 0;
        char separator = ';';
        String sepsearch = ";[^;]*;";
        outer: while(patch.hasNext()) {
            boolean skipThis = false;
            String patchID = null;
            while(patchID == null) {
                if(!patch.hasNext()) {
                    break outer;
                }
                
                String temp = patch.next();
                if(temp.toLowerCase().equals("separator")) {
                    separator = patch.next().charAt(0);
                    sepsearch = "\\" + separator + "[^\\" + separator + "]*\\" + separator;
                    patch.nextLine();
                    continue outer;
                } else if(temp.toLowerCase().equals("set")) {
                    if(skipThis) {
                        patch.nextLine();
                        continue outer;
                    }

                    temp = patch.next();
                    String func = patch.next().toLowerCase();
                    if(func.equals("true") || func.equals("1")) {
                        varNames.add(temp);
                        varVals.add(true);
                    } else if(func.equals("false") || func.equals("0")) {
                        varNames.add(temp);
                        varVals.add(false);
                    } else if(func.equals("hascode")) {
                        String str = patch.findInLine(sepsearch);
                        str = str.substring(1, str.length() - 1);
                        ByteBuffer bb = ByteBuffer.allocate((str.length()+1)/2);
                        bb.order(ByteOrder.LITTLE_ENDIAN);
                        int len = aobToByteBuffer(str, sepsearch, bb, bNames, bVals, EXPECT_NOTHING);
                        bb.position(0);
                        Code c1 = new Code(bb, len);
                        int count = 0;
                        for(int x = 0; x < objs.size(); x++) {
                            int num = objs.get(x).replaceCode(c1, c1, "", 0, 0);
                            if (num > 0) {
                                count += num;
                            }
                        }

                        varNames.add(temp);
                        varVals.add(count > 0);
                    } else if (func.equals("hasstring")) {
                        String str = patch.findInLine(sepsearch);
                        str = str.substring(1, str.length() - 1);
                        int count = 0;
                        for(int x = 0; x < objs.size(); x++) {
                            int num = objs.get(x).hasString(str);
                            if (num > 0) {
                                count += num;
                            }
                        }

                        varNames.add(temp);
                        varVals.add(count > 0);
                    } else {
                        System.err.println("ERROR: unrecognized set function.");
                        log.append("ERROR: unrecognized set function.\n");
                        patch.nextLine();
                    }
                    continue outer;
                } else if(temp.toLowerCase().equals("setbytes") || temp.toLowerCase().equals("setshorts")) {
                    boolean shorts = temp.toLowerCase().equals("setshorts");
                    if(skipThis) {
                        patch.nextLine();
                        continue outer;
                    }
                    
                    String tempName = patch.next();
                    String tempLine = patch.nextLine();
                    Scanner scanline = new Scanner(tempLine);
                    ArrayList<Integer> tempBytes = new ArrayList<Integer>();
                    
                    while(scanline.hasNext()) {
                        temp = scanline.next();
                        if(temp.startsWith("0x")) {
                            int val = Integer.parseInt(temp.substring(2), 16);
                            if(shorts) {
                                tempBytes.add(val & 0xff);
                                tempBytes.add((val & 0xff00) >> 8);
                            } else {
                                tempBytes.add(val);
                            }
                        } else if(temp.equals("nextobj")) {
                            int obj = 0;
                            for(int x = 0; x < objs.size(); x++) {
                                if(obj <= objs.get(x).id) {
                                    obj = objs.get(x).id + 1;
                                }
                            }
                            tempBytes.add(obj & 0xff);
                            tempBytes.add((obj & 0xff00) >> 8);
                        } else if(temp.equals("nextprop")) {
                            int prop = 0;
                            for(int x = 0; x < objs.size(); x++) {
                                int id = objs.get(x).maxPropRef();
                                if(prop <= id) {
                                    prop = id + 1;
                                }
                            }
                            
                            if(prop <= lastProp) {
                                prop = lastProp + 1;
                            }
                            lastProp = prop;
                            
                            tempBytes.add(prop & 0xff);
                            tempBytes.add((prop & 0xff00) >> 8);
                        } else if(temp.toLowerCase().equals("idbysdesc")) {
                            int id = 0;
                            temp = scanline.findInLine(sepsearch);
                            temp = temp.substring(1, temp.length() - 1);
                            for(int x = 0; x < objs.size(); x++) {
                                if(objs.get(x).type == 2) {
                                    ObjObject obj = (ObjObject) objs.get(x);
                                    for(int y = 0; y < obj.props.size(); y++) {
                                        if(obj.props.get(y).getID() == 8 &&obj.props.get(y).getType() == 9) {
                                            if(obj.props.get(y).string.equals(temp)) {
                                                id = obj.id;
                                            }
                                            break;
                                        }
                                    }
                                }
                            }
                            tempBytes.add(id & 0xff);
                            tempBytes.add((id & 0xff00) >> 8);
                        } else if(temp.equals("say")) {
                            tempBytes.add(0x1d);
                            temp = scanline.findInLine(sepsearch);
                            temp = temp.substring(1, temp.length() - 1);
                            tempBytes.add((temp.length() + 2) & 0xff);
                            tempBytes.add(((temp.length() + 2) & 0xff00) >> 8);
                            for(int x = 0; x < temp.length(); x++) {
                                tempBytes.add(temp.charAt(x) & 0xff);
                            }
                        } else if(temp.equals("stringl")) {
                            temp = scanline.findInLine(sepsearch);
                            temp = temp.substring(1, temp.length() - 1);
                            tempBytes.add((temp.length() + 2) & 0xff);
                            tempBytes.add(((temp.length() + 2) & 0xff00) >> 8);
                            for(int x = 0; x < temp.length(); x++) {
                                tempBytes.add(temp.charAt(x) & 0xff);
                            }
                        } else if(temp.equals("string")) {
                            temp = scanline.findInLine(sepsearch);
                            temp = temp.substring(1, temp.length() - 1);
                            for(int x = 0; x < temp.length(); x++) {
                                tempBytes.add(temp.charAt(x) & 0xff);
                            }
                        } else if(temp.equals("strlen")) {
                            temp = scanline.findInLine(sepsearch);
                            temp = temp.substring(1, temp.length() - 1);
                            tempBytes.add((temp.length() + 2) & 0xff);
                            tempBytes.add(((temp.length() + 2) & 0xff00) >> 8);
                        } else {
                            int val = Integer.parseInt(temp);
                            if(shorts) {
                                tempBytes.add(val & 0xff);
                                tempBytes.add((val & 0xff00) >> 8);
                            } else {
                                tempBytes.add(val);
                            }
                        }
                    }
                    
                    boolean overwrote = false;
                    for(int x = 0; x < bNames.size(); x++) {
                        if(bNames.get(x).equals(tempName)) {
                            bVals.set(x, tempBytes);
                            overwrote = true;
                            break;
                        }
                    }
                    if(!overwrote) {
                        bVals.add(tempBytes);
                        bNames.add(tempName);
                    }
                    continue outer;
                } else if(temp.toLowerCase().equals("setbyte")) {
                    if(skipThis) {
                        patch.nextLine();
                        continue outer;
                    }

                    temp = patch.next();
                    Integer val = patch.nextInt(16);
                    bNames.add(temp);
                    ArrayList<Integer> tempBytes = new ArrayList<Integer>();
                    tempBytes.add(val);
                    bVals.add(tempBytes);
                    continue outer;
                } else if(temp.toLowerCase().equals("echo")) {
                    if(skipThis) {
                        patch.nextLine();
                        continue outer;
                    }

                    String str = patch.nextLine();
                    System.out.println(str);
                    log.append(str);
                    log.append("\n");
                } else if(temp.startsWith("@")) {
                    int i = 1;
                    boolean target = true;
                    if(temp.charAt(1) == '!') {
                        i = 2;
                        target = false;
                    }
                    String str = temp.substring(i);
                    for(int x = varNames.size()-1; x >= 0; x--) {
                        if(varNames.get(x).equals(str)) {
                            skipThis = skipThis || (target != varVals.get(x));
                            break;
                        }
                    }
                } else {
                    patchID = temp;
                }
            }
            if(patchID.equals("!include")) {
                String incPatch = patch.nextLine().trim();
                File f1 = new File(patchpath);
                File f2 = new File(f1.getParentFile().getAbsolutePath(), incPatch);
                parsePatch(f2.getAbsolutePath(), null, null, log);
                if(patchFailed)
                    return;
                continue;
            } else if(skipThis || patchID.startsWith("//") || patchID.startsWith("#")) {
                patch.nextLine();
                if(!skipThis) {
                    //System.out.println("Skipping disabled command.");
                }
                continue;
            } else if(patchID.equals("exit")) {
                return;
            } else if(patchID.equals("abort")) {
                patchFailed = true;
                return;
            }

            String patchType = patch.next().toLowerCase();
            
            if(patchType.equals("setyear")) {
                int newYear = patch.nextInt();
                header[0x2a] = (byte) (((newYear / 1000) % 10) + '0');
                header[0x2b] = (byte) (((newYear / 100) % 10) + '0');
                header[0x2c] = (byte) (((newYear / 10) % 10) + '0');
                header[0x2d] = (byte) ((newYear % 10) + '0');
                if(this.verbose) {
                    System.out.println("Patch " + patchID + " replaced year.");
                    log.append("Patch " + patchID + " replaced year.\n");
                }
            } else if(patchType.equals("replacestring")) {
                int count = 0;
                String oldstr = patch.findInLine(sepsearch);
                oldstr = oldstr.substring(1, oldstr.length() - 1);
                String newstr = patch.findInLine(sepsearch);
                newstr = newstr.substring(1, newstr.length() - 1);
                for(int x = 0; x < objs.size(); x++) {
                    int num = objs.get(x).replaceString(oldstr, newstr);
                    if (num > 0) {
                        count += num;
                    }
                }
                totalChanges += count;
                if(this.verbose) {
                    System.out.println("Patch " + patchID + " replaced " + count + " strings.");
                    log.append("Patch " + patchID + " replaced " + count + " strings.\n");
                }
            } else if(patchType.equals("replacepropdef")) {
                int count = 0;
                int oldprop = patch.nextInt();
                int newprop = patch.nextInt();
                for(int x = 0; x < objs.size(); x++) {
                    int num = objs.get(x).replacePropDefID(oldprop, newprop);
                    if (num > 0) {
                        count += num;
                    }
                }
                totalChanges += count;
                if(this.verbose) {
                    System.out.println("Patch " + patchID + " replaced " + count + " prop defs.");
                    log.append("Patch " + patchID + " replaced " + count + " prop defs.\n");
                }
            } else if(patchType.equals("replacepropuse")) {
                int count = 0;
                int oldprop = patch.nextInt();
                int newprop = patch.nextInt();
                for(int x = 0; x < objs.size(); x++) {
                    int num = objs.get(x).replacePropUseID(oldprop, newprop);
                    if (num > 0) {
                        count += num;
                    }
                }
                totalChanges += count;
                if(this.verbose) {
                    System.out.println("Patch " + patchID + " replaced " + count + " prop uses.");
                    log.append("Patch " + patchID + " replaced " + count + " prop uses.\n");
                }
            } else if(patchType.equals("insertcodeafter")) {
                int count = 0;
                String oldstr = patch.findInLine(sepsearch);
                oldstr = oldstr.substring(1, oldstr.length() - 1);
                String newstr = patch.findInLine(sepsearch);
                newstr = newstr.substring(1, newstr.length() - 1);
                ByteBuffer bb = ByteBuffer.allocate(1024);
                bb.order(ByteOrder.LITTLE_ENDIAN);
                int len = aobToByteBuffer(oldstr, sepsearch, bb, bNames, bVals, EXPECT_NOTHING);
                bb.position(0);
                Code c1 = new Code(bb, len);
                bb = ByteBuffer.allocate(1024);
                bb.order(ByteOrder.LITTLE_ENDIAN);
                len = aobToByteBuffer(newstr, sepsearch, bb, bNames, bVals, EXPECT_NOTHING);
                bb.position(0);
                Code c2 = new Code(bb, len);
                for(int x = 0; x < objs.size(); x++) {
                    int num = objs.get(x).insertCodeAfter(c1, c2);
                    if (num > 0) {
                        count += num;
                    }
                }
                totalChanges += count;
                if(this.verbose) {
                    System.out.println("Patch " + patchID + " inserted " + count + " code segments.");
                    log.append("Patch " + patchID + " inserted " + count + " code segments.\n");
                }
            } else if(patchType.equals("insertcodebefore")) {
                int count = 0;
                String oldstr = patch.findInLine(sepsearch);
                oldstr = oldstr.substring(1, oldstr.length() - 1);
                String newstr = patch.findInLine(sepsearch);
                newstr = newstr.substring(1, newstr.length() - 1);
                ByteBuffer bb = ByteBuffer.allocate(1024);
                bb.order(ByteOrder.LITTLE_ENDIAN);
                int len = aobToByteBuffer(oldstr, sepsearch, bb, bNames, bVals, EXPECT_NOTHING);
                bb.position(0);
                Code c1 = new Code(bb, len);
                bb = ByteBuffer.allocate(1024);
                bb.order(ByteOrder.LITTLE_ENDIAN);
                len = aobToByteBuffer(newstr, sepsearch, bb, bNames, bVals, EXPECT_NOTHING);
                bb.position(0);
                Code c2 = new Code(bb, len);
                for(int x = 0; x < objs.size(); x++) {
                    int num = objs.get(x).insertCodeBefore(c1, c2);
                    if (num > 0) {
                        count += num;
                    }
                }
                totalChanges += count;
                if(this.verbose) {
                    System.out.println("Patch " + patchID + " inserted " + count + " code segments.");
                    log.append("Patch " + patchID + " inserted " + count + " code segments.\n");
                }
            } else if(patchType.equals("prependpropcode")) {
                int count = 0;
                int prop = 0;
                if(patch.hasNextInt()) {
                    prop = patch.nextInt();
                } else {
                    String temp = patch.next();
                    if(temp.startsWith("@")) {
                        String str = temp.substring(1);
                        for(int x = bNames.size()-1; x >= 0; x--) {
                            if(bNames.get(x).equals(str)) {
                                if(bVals.get(x).size() != 2) {
                                    System.err.println("Error code 563: expected 2-byte prop.");
                                }
                                prop = bVals.get(x).get(0) + (bVals.get(x).get(1) << 8);
                            }
                        }
                    } else {
                        System.err.println("Not a prop? error code 573");
                    }
                }
                
                String str = patch.findInLine(sepsearch);
                str = str.substring(1, str.length() - 1);
                ByteBuffer bb = ByteBuffer.allocate(1024);
                bb.order(ByteOrder.LITTLE_ENDIAN);
                int len = aobToByteBuffer(str, sepsearch, bb, bNames, bVals, EXPECT_NOTHING);
                bb.position(0);
                Code c = new Code(bb, len);
                
                for(int x = 0; x < objs.size(); x++) {
                    int num = objs.get(x).prependPropCode(prop, c);
                    if (num > 0) {
                        count += num;
                    }
                }
                totalChanges += count;
                if(this.verbose) {
                    System.out.println("Patch " + patchID + " prepended " + count + " code segments.");
                    log.append("Patch " + patchID + " prepended " + count + " code segments.\n");
                }
            } else if(patchType.equals("replacecode")) {
                int count = 0;
                String oldstr = patch.findInLine(sepsearch);
                oldstr = oldstr.substring(1, oldstr.length() - 1);
                String reptype = patch.next();
                int repindex = patch.nextInt();
                //int repcount = patch.nextInt();
                String newstr = patch.findInLine(sepsearch);
                newstr = newstr.substring(1, newstr.length() - 1);
                ByteBuffer bb = ByteBuffer.allocate(1024);
                bb.order(ByteOrder.LITTLE_ENDIAN);
                int len = aobToByteBuffer(oldstr, sepsearch, bb, bNames, bVals, EXPECT_NOTHING);
                bb.position(0);
                Code c1 = new Code(bb, len);
                bb = ByteBuffer.allocate(1024);
                bb.order(ByteOrder.LITTLE_ENDIAN);
                len = aobToByteBuffer(newstr, sepsearch, bb, bNames, bVals, EXPECT_NOTHING);
                bb.position(0);
                Code c2 = new Code(bb, len);
                for(int x = 0; x < objs.size(); x++) {
                    int num = objs.get(x).replaceCode(c1, c2, reptype, repindex, c2.instructions.size());
                    if (num > 0) {
                        count += num;
                    }
                }
                totalChanges += count;
                if(this.verbose) {
                    System.out.println("Patch " + patchID + " replaced " + count + " code segments.");
                    log.append("Patch " + patchID + " replaced " + count + " code segments.\n");
                }
            } else if(patchType.equals("addvoc")) {
                String word1 = patch.findInLine(sepsearch);
                word1 = word1.substring(1, word1.length() - 1);
                String word2 = patch.findInLine(sepsearch);
                word2 = word2.substring(1, word2.length() - 1);
                
                short pNum = toShort(patch.findInLine(sepsearch), bNames, bVals);
                short oNum = toShort(patch.findInLine(sepsearch), bNames, bVals);
                short cFlag = toShort(patch.findInLine(sepsearch), bNames, bVals);
                voc.add(new Vocab(word1, word2, pNum, oNum, cFlag));
                
                totalChanges++;
                if(this.verbose) {
                    System.out.println("Patch " + patchID + " added voc string: " + word1 + word2 + ".");
                    log.append("Patch " + patchID + " added voc string: " + word1 + word2 + ".\n");
                }
            } else if(patchType.equals("addobject")) {
                short oNum = toShort(patch.findInLine(sepsearch), bNames, bVals);
                objs.add(new ObjObject(oNum));
                
                totalChanges++;
                if(this.verbose) {
                    System.out.println("Patch " + patchID + " added object " + oNum + ".");
                    log.append("Patch " + patchID + " added object " + oNum + ".\n");
                }
            } else if(patchType.equals("addprop")) {
                String objid_aob = patch.findInLine(sepsearch);
                short objid = toShort(objid_aob, bNames, bVals);
                String aob = patch.findInLine(sepsearch);
                aob = aob.substring(1, aob.length() - 1);

                ByteBuffer bb = ByteBuffer.allocate(16384);
                bb.order(ByteOrder.LITTLE_ENDIAN);
                aobToByteBuffer(aob, sepsearch, bb, bNames, bVals, EXPECT_PROP);
                bb.position(0);
                
                int count = 0;
                Prop prop = new Prop(bb.getShort(), bb.get(), bb.getShort(), bb.get(), bb);
                for(int x = 0; x < objs.size(); x++) {
                    if(objs.get(x).id == objid) {
                        objs.get(x).addProp(prop);
                        count++;
                    }
                }
                totalChanges += count;
                if(this.verbose) {
                    System.out.println("Patch " + patchID + " added " + count + " props.");
                    log.append("Patch " + patchID + " added " + count + " props.\n");
                }
            } else if(patchType.equals("addsuper")) {
                short objid = toShort(patch.findInLine(sepsearch), bNames, bVals);
                short superid = toShort(patch.findInLine(sepsearch), bNames, bVals);
                
                int count = 0;
                for(int x = 0; x < objs.size(); x++) {
                    if(objs.get(x).id == objid) {
                        ObjObject obj = (ObjObject)objs.get(x);
                        obj.supers.add((int)superid);
                        count++;
                    }
                }
                totalChanges += count;
                if(this.verbose) {
                    System.out.println("Patch " + patchID + " added " + count + " supers.");
                    log.append("Patch " + patchID + " added " + count + " supers.\n");
                }
            } else {
                System.out.println("Error: patch " + patchID + " has unrecognized type.");
                log.append("Error: patch " + patchID + " has unrecognized type.\n");
            }

            if(patch.hasNext())
                patch.nextLine();
        }
        if(this.verbose) {
            System.out.println(totalChanges + " changes made.");
            log.append(totalChanges + " changes made.\n");
        }
    }

    private void fixAddresses(ArrayList<Resource> resources) {
        for(int x = 0; x < resources.size(); x++) {
            resources.get(x).fixAddresses();
        }
    }
}
