package tads2manip;

import java.io.IOException;
import java.nio.ByteBuffer;
import java.util.ArrayList;

public abstract class Obj {
    public int type;//1 byte
    public int id;//2 bytes
    public int orgSize;//2 bytes; potential size in RAM?
    public int orgUse;//2 bytes; size on disk?
    public ArrayList<Prop> props;
    public byte[] data;

    public int writeSize;
    public int writeUse;
    public int writeAddr;

    public Obj(int type, int id, int orgSize, int orgUse) {
        this.type = type;
        this.id = id;
        this.orgSize = orgSize;
        this.orgUse = orgUse;
        props = new ArrayList<Prop>();
        data = null;
    }

    public abstract int hasString(String oldstr);
    public abstract int replaceString(String oldstr, String newstr);
    public abstract int replacePropDefID(int id1, int id2);
    public abstract int maxPropRef();
    public abstract int replacePropUseID(int id1, int id2);
    public abstract int insertCodeAfter(Code c1, Code c2);
    public abstract int insertCodeBefore(Code c1, Code c2);
    public abstract int prependPropCode(int prop, Code c);
    public abstract int replaceCode(Code c1, Code c2, String rep, int index, int count);
    public abstract void findTextMatches(java.util.List<String> matches, String text, boolean set);
    public void addProp(Prop prop) {
        System.err.println("ERROR: Trying to add prop to wrong object type.");
    }

    public void fixAddresses() {}

    public abstract void parseObj(ByteBuffer bb, CryptSettings crypt) throws IOException;
    abstract int getSize();
    abstract void write(ByteBuffer bb, int baseaddr, CryptSettings crypt);
}
