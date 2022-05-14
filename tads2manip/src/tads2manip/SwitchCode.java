package tads2manip;

import java.nio.ByteBuffer;
import java.util.ArrayList;

public class SwitchCode extends CodeCommon  {
    SwitchTable table;
    int realoffset = -9999;
    int codeoffset = -9999;

    public SwitchCode(ByteBuffer bb, int offsetOrg) {
        super();
        instructions = new ArrayList<Instruction>();

        int codeLength = bb.getShort()-2;

        codeoffset = bb.position();
        while(bb.position() < codeoffset+codeLength) {
            Instruction inst = new Instruction(bb, bb.position() - offsetOrg, offsetOrg, codeoffset+codeLength);
            instructions.add(inst);
        }

        realoffset = offsetOrg;

        table = new SwitchTable();
        short numCases = bb.getShort();
        for(int x = 0; x < numCases; x++) {
            table.cases.add(new SwitchCase(bb, bb.position()-offsetOrg, false));
        }
        table.cases.add(new SwitchCase(bb, bb.position()-offsetOrg, true));
    }

    public void findTextMatches(java.util.List<String> matches, String text, boolean set) {
        for(int x = 0; x < instructions.size(); x++) {
            instructions.get(x).findTextMatches(matches, text, set);
        }
        for(int x = 0; x < table.cases.size(); x++) {
            table.cases.get(x).findTextMatches(matches, text, set);
        }
    }

    public int hasString(String str) {
        int count = 0;
        for(int x = 0; x < instructions.size(); x++) {
            count += instructions.get(x).hasString(str);
        }
        for(int x = 0; x < table.cases.size(); x++) {
            count += table.cases.get(x).hasString(str);
        }
        return count;
    }

    public int replaceString(String oldstr, String newstr) {
        int count = 0;
        for(int x = 0; x < instructions.size(); x++) {
            count += instructions.get(x).replaceString(oldstr, newstr);
        }
        for(int x = 0; x < table.cases.size(); x++) {
            count += table.cases.get(x).replaceString(oldstr, newstr);
        }
        return count;
    }

    public int replacePropUseID(int id1, int id2) {
        int count = 0;
        for(int x = 0; x < instructions.size(); x++) {
            count += instructions.get(x).replacePropUseID(id1, id2);
        }
        for(int x = 0; x < table.cases.size(); x++) {
            count += table.cases.get(x).replacePropUseID(id1, id2);
        }
        return count;
    }
    
    public int maxPropRef() {
        int c = 0;
        for(int x = 0; x < instructions.size(); x++) {
            c = Math.max(c, instructions.get(x).maxPropRef());
        }
        for(int x = 0; x < table.cases.size(); x++) {
            c = Math.max(c, table.cases.get(x).maxPropRef());
        }
        return c;
    }

    public boolean setJumpTargets(ArrayList<ArrayList<Instruction>> insts) {
        boolean fail = false;

        insts.add(instructions);

        for(int x = 0; x < instructions.size(); x++) {
            instructions.get(x).setJmpTarget(insts, realoffset, true);
        }

        for(int x = 0; x < table.cases.size(); x++) {
            if(table.cases.get(x).target == null) {
                boolean foundthisone = false;
                int tgt = table.cases.get(x).myAddr + (table.cases.get(x).getSize() - 2) + table.cases.get(x).orgJump;

                for(int y = 0; y < insts.size() && !foundthisone; y++) {
                    for(int z = 0; z < insts.get(y).size(); z++) {
                        if(insts.get(y).get(z).addr == tgt) {
                            foundthisone = true;
                            table.cases.get(x).target = insts.get(y).get(z);
                            //table.cases.get(x).targetorgaddr = insts.get(y).get(z).addr;
                            break;
                        }
                    }
                }

                if(!foundthisone) {
                    System.err.println("ERROR: Missing case target.");
                }
            }
        }

        insts.remove(instructions);
        return fail;
    }

    //TODO: figure out why addresses are wrong after parsing (before running this)
    void fixAddresses(int off) {
        int pos = off + 2;
        for(int x = 0; x < instructions.size(); x++) {
            //if(table.cases.get(0).debugid == 1937)System.err.println(instructions.get(x).addr + " -> " + pos);
            instructions.get(x).addr = pos;
            instructions.get(x).fixAddresses();
            pos += instructions.get(x).getSize();
        }
        pos += 2;//a short for numcases
        for(int x = 0; x < table.cases.size(); x++) {
            //if(table.cases.get(x).debugid == 1937)System.err.println(table.cases.get(x).myAddr + " -> " + pos);
            table.cases.get(x).myAddr = pos;
            pos += table.cases.get(x).getSize();
        }
    }

    public int getSize() {
        int size = 4;//code length and num cases
        for(int x = 0; x < instructions.size(); x++) {
            size += instructions.get(x).getSize();
        }
        for(int x = 0; x < table.cases.size(); x++) {
            size += table.cases.get(x).getSize();
        }
        return size;
    }

    public void write(ByteBuffer bb) {
        int isize = 0;
        for(int x = 0; x < instructions.size(); x++) {
            isize += instructions.get(x).getSize();
        }

        bb.putShort((short)(isize + 2));
        for(int x = 0; x < instructions.size(); x++) {
            instructions.get(x).write(bb);
        }
        bb.putShort((short)(table.cases.size() - 1));//num cases, not counting default
        for(int x = 0; x < table.cases.size(); x++) {
            table.cases.get(x).write(bb);
        }
    }
}
