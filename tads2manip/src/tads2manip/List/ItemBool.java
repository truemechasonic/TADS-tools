package tads2manip.List;

import java.nio.ByteBuffer;

public class ItemBool extends ListItem {

    public ItemBool(int type) {
        this.type = type;
    }

    @Override
    public void parse(ByteBuffer bb) {}

    public void findTextMatches(java.util.List<String> matches, String text, boolean set) {

    }

    public int hasString(String str) {
        return 0;
    }
    public int replaceString(String oldstr, String newstr) {
        return 0;
    }
    public int replacePropUseID(int id1, int id2) {
        return 0;
    }
    public int maxPropRef() {
       return 0;
    }

    public boolean equalsItem(ListItem item) {
        if(type != item.type) {
            return false;
        }
        return true;
    }

    @Override
    public void write(ByteBuffer bb) {
        bb.put((byte)type);
    }

    @Override
    public int getSize() {
        return 1;
    }

}
