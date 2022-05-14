package tads2manip.List;

import tads2manip.Prop;
import java.nio.ByteBuffer;

public class ItemNum extends ListItem {

    int num;
    boolean switchcase = false;

    public ItemNum(int type) {
        this.type = type;
    }

    public ItemNum(boolean switchcase, int type) {
        this.type = type;
        this.switchcase = switchcase;
    }

    @Override
    public void parse(ByteBuffer bb) {
        if(switchcase) {
            switch (type) {
                case 0x1://pushnum
                    num = bb.getInt();
                    break;
                case 0x2://pushobj
                case 0x43://pushpn
                    num = (int)bb.getShort();
                    break;
                default:
                    System.err.println("WTF in switchclass itemnum " + type);
            }
        }
        else {
            switch (type) {
                case Prop.NUM:
                    num = bb.getInt();
                    break;
                case Prop.OBJECT:
                case Prop.PROPNUM:
                    num = (int)bb.getShort();
                    break;
                default:
                    System.err.println("WTF in class itemnum " + type);
            }
        }
    }

    public void findTextMatches(java.util.List<String> matches, String text, boolean set) {

    }

    public int hasString(String str) {
        return 0;
    }
    public int replaceString(String oldstr, String newstr) {
        return 0;
    }
    public int replacePropUseID(int id1, int id2) {
        if(!switchcase && type == Prop.PROPNUM) {
            if(num == id1) {
                num = id2;
                return 1;
            }
        } else if(switchcase && type == 0x43) {
            if(num == id1) {
                num = id2;
                return 1;
            }
        }
        return 0;
    }
    
    public int maxPropRef() {
       int c = 0;
       if(!switchcase && type == Prop.PROPNUM) {
            c = Math.max(c, num);
        } else if(switchcase && type == 0x43) {
            c = Math.max(c, num);
        }
       return c;
    }

    public boolean equalsItem(ListItem item) {
        if(type != item.type) {
            return false;
        }
        ItemNum inum = (ItemNum) item;
        return (inum.num == num);
    }

    @Override
    public void write(ByteBuffer bb) {
        bb.put((byte)type);
        if(switchcase) {
            switch (type) {
                case 0x1://pushnum:
                    bb.putInt(num);
                    break;
                case 0x2://pushobj
                case 0x43://pushpn
                    bb.putShort((short)num);
                    break;
                default:
                    System.err.println("WTF in switchclass itemnum " + type);
            }
        }
        else {
            switch (type) {
                case Prop.NUM:
                    bb.putInt(num);
                    break;
                case Prop.OBJECT:
                case Prop.PROPNUM:
                    bb.putShort((short)num);
                    break;
                default:
                    System.err.println("WTF in class itemnum " + type);
            }
        }
    }

    @Override
    public int getSize() {
        if(switchcase) {
            switch (type) {
                case 0x1://pushnum:
                    return 5;
                case 0x2://pushobj
                case 0x43://pushpn
                    return 3;
                default:
                    System.err.println("WTF in switchclass itemnum " + type);
            }
        }
        else {
            switch (type) {
                case Prop.NUM:
                    return 5;
                case Prop.OBJECT:
                case Prop.PROPNUM:
                    return 3;
                default:
                    System.err.println("WTF in class itemnum " + type);
            }
        }
        return -1;
    }
}
