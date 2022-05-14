package tads2manip.List;

import tads2manip.Prop;
import java.nio.ByteBuffer;
import java.util.ArrayList;

public class List extends ListItem {
    public ArrayList<ListItem> items;
    public boolean mainList = false;

    public List(boolean outermost) {
        mainList = outermost;
        items = new ArrayList<ListItem>();
    }

    @Override
    public void parse(ByteBuffer bb) {
        int size = ((int)bb.getShort())-2;

        int addr = bb.position();
        while(bb.position() < addr + size) {
            byte itemType = bb.get();
            ListItem item = null;
            switch(itemType) {
                case Prop.NUM:
                    item = new ItemNum(Prop.NUM);
                    break;
                case Prop.OBJECT:
                    item = new ItemNum(Prop.OBJECT);
                    break;
                case Prop.SSTRING:
                    item = new ItemString(Prop.SSTRING, true);
                    break;
                case Prop.NIL:
                    item = new ItemBool(Prop.NIL);
                    break;
                case Prop.TRUE:
                    item = new ItemBool(Prop.TRUE);
                    break;
                case Prop.LIST:
                    item = new List(false);
                    break;
                case Prop.DSTRING:
                    item = new ItemString(Prop.DSTRING, true);
                    break;
                case Prop.PROPNUM:
                    item = new ItemNum(Prop.PROPNUM);
                    break;
                default:
                    System.err.println("OH NO missing list item " + itemType);
            }

            item.parse(bb);
            items.add(item);
        }
    }

    public void findTextMatches(java.util.List<String> matches, String text, boolean set) {
        if(items != null) {
            for(int x = 0; x < items.size(); x++) {
                items.get(x).findTextMatches(matches, text, set);
            }
        }
    }

    public int hasString(String str) {
        int count = 0;
        if(items != null) {
            for(int x = 0; x < items.size(); x++) {
                count += items.get(x).hasString(str);
            }
        }
        return count;
    }
    public int replaceString(String oldstr, String newstr) {
        int count = 0;
        if(items != null) {
            for(int x = 0; x < items.size(); x++) {
                count += items.get(x).replaceString(oldstr, newstr);
            }
        }
        return count;
    }
    public int replacePropUseID(int id1, int id2) {
        int count = 0;
        if(items != null) {
            for(int x = 0; x < items.size(); x++) {
                count += items.get(x).replacePropUseID(id1, id2);
            }
        }
        return count;
    }
    
    public int maxPropRef() {
        int c = 0;
        if(items != null) {
            for(int x = 0; x < items.size(); x++) {
                c = Math.max(c, items.get(x).maxPropRef());
            }
        }
        return c;
    }
    
    public boolean equalsList(ListItem listi) {
        if(type != listi.type) {
            return false;
        }
        List list = (List) listi;
        if(items.size() != list.items.size()) {
            return false;
        }
        for(int x = 0; x < items.size(); x++) {
            if(!items.get(x).equalsItem(list.items.get(x))) {
                return false;
            }
        }
        return true;
    }
    public boolean equalsItem(ListItem item) {
        return equalsList(item);
    }

    @Override
    public void write(ByteBuffer bb) {
        int size = getSize();
        if(!mainList) {
            bb.put((byte)Prop.LIST);
            size--;
        }


        bb.putShort((short)size);
        size -= 2;

        for(int x = 0; x < items.size(); x++) {
            items.get(x).write(bb);
        }
    }

    @Override
    public int getSize() {
        int size = 2;
        if(!mainList) {
            size += 1;
        }
        if(items != null) {
            for(int x = 0; x < items.size(); x++) {
                size += items.get(x).getSize();
            }
        }
        return size;
    }
}
