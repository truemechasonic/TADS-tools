package tads2manip;

import tads2manip.List.ItemBool;
import tads2manip.List.ItemNum;
import tads2manip.List.ItemString;
import tads2manip.List.ListItem;
import java.nio.ByteBuffer;

class SwitchCase {
    static int debugidg = 0;
    public int debugid;//1937

    int type = -1;
    ListItem item = null;

    int myAddr;

    short orgJump;
    int targetorgaddr;

    Instruction target = null;

    public SwitchCase(ByteBuffer bb, int addr, boolean defcase) {
        debugid = debugidg++;
        myAddr = addr;
        if(!defcase) {
            type = bb.get();
            switch(type) {
                case 0x1://pushnum
                    item = new ItemNum(true, 0x1);
                    break;
                case 0x2://pushobj
                    item = new ItemNum(true, 0x2);
                    break;
                case 0x1c://pushself
                    item = new ItemBool(0x1c);
                    break;
                case 0x1f://pushstr
                    item = new ItemString(0x1f, true);
                    break;
                case 0x21://pushnil
                    item = new ItemBool(0x21);
                    break;
                case 0x22://pushtrue
                    item = new ItemBool(0x22);
                    break;
                case 0x43://pushpn
                    item = new ItemNum(true, 0x43);
                    break;
                default:
                    System.err.println("OH NO missing list item " + Integer.toHexString(type));
            }
            item.parse(bb);
        }
        orgJump = bb.getShort();
        //System.out.println(debugid + ": " + Integer.toHexString(orgJump));
    }

    public void write(ByteBuffer bb) {
        if(item != null) {
            item.write(bb);
        }

        bb.putShort((short)(target.addr - myAddr - (getSize() - 2)));
    }

    public int getSize() {
        if(item == null) {
            return 2;
        }
        else {
            return item.getSize() + 2;
        }
    }

    public void findTextMatches(java.util.List<String> matches, String text, boolean set) {
        if(item != null) {
            item.findTextMatches(matches, text, set);
        }
    }

    public int hasString(String str) {
        if(item != null) {
            return item.hasString(str);
        }
        return 0;
    }

    int replaceString(String oldstr, String newstr) {
        if(item != null) {
            return item.replaceString(oldstr, newstr);
        }
        return 0;
    }

    int replacePropUseID(int id1, int id2) {
        if(item != null) {
            return item.replacePropUseID(id1, id2);
        }
        return 0;
    }
    
    int maxPropRef() {
        if(item != null) {
            return item.maxPropRef();
        }
        return 0;
    }
}
