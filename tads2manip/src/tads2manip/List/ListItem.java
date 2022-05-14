package tads2manip.List;

import java.nio.ByteBuffer;

public abstract class ListItem {
    int type = -1;

    public abstract void parse(ByteBuffer bb);

    public abstract void findTextMatches(java.util.List<String> matches, String text, boolean set);
    public abstract int hasString(String str);
    public abstract int replaceString(String oldstr, String newstr);
    public abstract int replacePropUseID(int id1, int id2);
    public abstract int maxPropRef();

    public abstract void write(ByteBuffer bb);
    public abstract int getSize();

    public abstract boolean equalsItem(ListItem item);
}
