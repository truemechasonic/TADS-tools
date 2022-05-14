package tads2manip;

import java.nio.ByteBuffer;
import java.util.ArrayList;

public class Code extends CodeCommon {
    byte[] garbage = null;

    int offset = 0;

    protected Code() {}

    public Code(ByteBuffer bb, int length) {
        instructions = new ArrayList<Instruction>();

        offset = bb.position();
        while(bb.position() < offset+length) {
            Instruction inst = new Instruction(bb, bb.position() - offset, offset, offset+length);
            instructions.add(inst);

            if(inst.opcode == 0x16 && bb.position() < offset+length) {
                boolean morejumps = setJumpTargets(false);
                if(!morejumps) {
                    garbage = new byte[offset+length - bb.position()];
                    bb.get(garbage, 0, offset+length - bb.position());
                }
            }
        }

        fixAddresses();
        setJumpTargets(true);
    }

    public int hasString(String str) {
        int count = 0;
        for(int x = 0; x < instructions.size(); x++) {
            count += instructions.get(x).hasString(str);
        }
        return count;
    }

    public int replaceString(String oldstr, String newstr) {
        int count = 0;
        for(int x = 0; x < instructions.size(); x++) {
            count += instructions.get(x).replaceString(oldstr, newstr);
        }
        return count;
    }

    public int replacePropUseID(int id1, int id2) {
        int count = 0;
        for(int x = 0; x < instructions.size(); x++) {
            count += instructions.get(x).replacePropUseID(id1, id2);
        }
        return count;
    }
    
    public int maxPropRef() {
        int c = 0;
        for(int x = 0; x < instructions.size(); x++) {
            c = Math.max(c, instructions.get(x).maxPropRef());
        }
        return c;
    }

    public void findTextMatches(java.util.List<String> matches, String text, boolean set) {
        for(int x = 0; x < instructions.size(); x++) {
            instructions.get(x).findTextMatches(matches, text, set);
        }
    }

    private boolean setJumpTargets(boolean doSwitches) {
        ArrayList<ArrayList<Instruction>> insts = new ArrayList<ArrayList<Instruction>>();
        insts.add(instructions);

        boolean fail = false;
        for(int x = 0; x < instructions.size(); x++) {
            fail = fail || instructions.get(x).setJmpTarget(insts, offset, doSwitches);
        }
        return fail;
    }

    public void fixAddresses() {
        int pos = offset;
        for(int x = 0; x < instructions.size(); x++) {
            instructions.get(x).addr = pos;
            instructions.get(x).fixAddresses();
            pos += instructions.get(x).getSize();
        }
    }

    public int getSize() {
        int size = 0;
        for(int x = 0; x < instructions.size(); x++) {
            size += instructions.get(x).getSize();
        }
        if(garbage != null) {
            size += garbage.length;
        }
        return size;
    }

    public void write(ByteBuffer bb) {
        for(int x = 0; x < instructions.size(); x++) {
            instructions.get(x).write(bb);
        }
        if(garbage != null) {
            bb.put(garbage, 0, garbage.length);
        }
    }

    static Code duplicate(Code org) {
        Code code = new Code();
        code.instructions = new ArrayList<Instruction>();
        for(int x = 0; x < org.instructions.size(); x++) {
            code.instructions.add(org.instructions.get(x).duplicate());
        }
        for(int x = 0; x < org.instructions.size(); x++) {
            if(org.instructions.get(x).jmptarget != null) {
                for(int y = 0; y < org.instructions.size(); y++) {
                    if(org.instructions.get(x).jmptarget == org.instructions.get(y)) {
                        code.instructions.get(x).jmptarget = code.instructions.get(y);
                    }
                }
            }
        }
        return code;
    }

    /*void printHex() {
        System.err.print("\t");
        for(int x = 0; x < instructions.size(); x++) {
            instructions.get(x).printHex();
            System.err.print("| ");
        }
        System.err.println();
    }*/
}
