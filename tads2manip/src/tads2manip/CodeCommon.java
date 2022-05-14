package tads2manip;

import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.util.ArrayList;
import java.util.Set;

public abstract class CodeCommon {
    public ArrayList<Instruction> instructions;

    public int insertCodeAfter(Code c1, Code c2) {
        int count = 0;
        for(int x = 0; x < instructions.size(); x++) {
            count += instructions.get(x).insertCodeAfter(c1, c2);
        }

        for(int x = 0; x < instructions.size(); x++) {
            boolean good = true;
            int extra = 0;
            for(int y = 0; y < c1.instructions.size() + extra; y++) {
                if(x+y+extra >= instructions.size()) {
                    good = false;
                    break;
                }
                else if(instructions.get(x+y).opcode == 0x4e) {
                    if(c1.instructions.get(y - extra).opcode != instructions.get(x+y).opcode) {
                        extra++;
                    }
                }
                else if(instructions.get(x+y).jmptarget != null) {
                    if(c1.instructions.get(y - extra).opcode != instructions.get(x+y).opcode) {
                        good = false;
                        break;
                    }
                }
                else if(!instructions.get(x+y).equalsInstruction(c1.instructions.get(y - extra))) {
                    good = false;
                    break;
                }
            }
            //if(good && extra > 0) System.err.println("Ugh.");

            if(good) {
                Code cd = Code.duplicate(c2);
                x += c1.instructions.size() + extra;
                for(int y = 0; y < cd.instructions.size(); y++) {
                    instructions.add(x, cd.instructions.get(y));
                    x++;
                }
                count++;
            }
        }
        return count;
    }
    
    //Note: If c1 is null, it just inserts c2 at the start of this code
    public int insertCodeBefore(Code c1, Code c2) {
        int count = 0;
        if(c1 != null) {
            for(int x = 0; x < instructions.size(); x++) {
                count += instructions.get(x).insertCodeBefore(c1, c2);
            }
        }
        
        for(int x = 0; x < instructions.size(); x++) {
            boolean good = true;
            int extra = 0;
            if(c1 == null) {
                if(instructions.get(x).opcode == 0x4d || instructions.get(x).opcode == 0x18 || instructions.get(x).opcode == 0x4e) {
                    good = false;
                }
            } else {
                for(int y = 0; y < c1.instructions.size() + extra; y++) {
                    if(x+y+extra >= instructions.size()) {
                        good = false;
                        break;
                    } else if(instructions.get(x+y).opcode == 0x4e) {
                        if(c1.instructions.get(y - extra).opcode != instructions.get(x+y).opcode) {
                            extra++;
                        }
                    } else if(instructions.get(x+y).jmptarget != null) {
                        if(c1.instructions.get(y - extra).opcode != instructions.get(x+y).opcode) {
                            good = false;
                            break;
                        }
                    } else if(!instructions.get(x+y).equalsInstruction(c1.instructions.get(y - extra))) {
                        good = false;
                        break;
                    }
                }
            }

            if(good) {
                Code cd = Code.duplicate(c2);
                x += extra;
                for(int y = 0; y < cd.instructions.size(); y++) {
                    instructions.add(x, cd.instructions.get(y));
                    x++;
                }
                count++;
                
                if(c1 == null) {
                    return count;
                }
            }
        }
        return count;
    }
    
    public int replaceCode(Code c1, Code c2, String rep, int index, int count) {
        int c = 0;
        for(int x = 0; x < instructions.size(); x++) {
            c += instructions.get(x).replaceCode(c1, c2, rep, index, count);
        }

        for(int x = 0; x < instructions.size(); x++) {
            boolean good = true;
            int extra = 0;
            for(int y = 0; y < c1.instructions.size() + extra; y++) {
                if(x+y >= instructions.size()) {
                    good = false;
                    break;
                }
                else if(instructions.get(x+y).opcode == 0x4e && y > 0) {
                    if(c1.instructions.get(y - extra).opcode != instructions.get(x+y).opcode) {
                        extra++;
                    }
                }
                else if(!instructions.get(x+y).equalsInstruction(c1.instructions.get(y - extra))) {
                    good = false;
                    break;
                }
            }

            if(good) {
                int extra2 = 0;
                int xreal = x;
                for(int y = 0; y < count + extra2; y++) {
                    if(x+index >= x+c1.instructions.size()) {
                        System.err.println("WARNING: Code Replace index beyond strict bounds.");
                    }
                    if(instructions.get(x+index).opcode != 0x4e) {
                        instructions.get(x+index).replaceInstruction(rep, c2.instructions.get(y-extra2));
                        if(c2.instructions.get(y-extra2).jmptarget != null) {
                            int extra3 = 0;
                            for(int z = 0; z < count + extra3; z++) {
                                if(c2.instructions.get(z-extra3) == c2.instructions.get(y-extra2).jmptarget) {
                                    instructions.get(x+index).jmptarget = instructions.get(xreal+z+index);
                                    break;
                                }
                                if(instructions.get(xreal+z+index).opcode == 0x4e) {
                                    extra3++;
                                }
                            }
                        }
                        else {
                            instructions.get(x+index).jmptarget = null;
                        }
                    }
                    else {
                        extra2++;
                    }
                    x++;
                }
                c++;
                x += c1.instructions.size() + extra;
            }
        }
        return c;
    }
}
