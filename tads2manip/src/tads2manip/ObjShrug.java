package tads2manip;

import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;

public class ObjShrug extends Obj {
    public ObjShrug(int type, int id, int orgSize, int orgUse) {
        super(type,id,orgSize,orgUse);
    }

    public void parseObj(ByteBuffer bb, CryptSettings crypt) throws IOException {
        crypt.crypt(bb, orgUse, true);
        data = new byte[orgUse];
        bb.get(data, 0, orgUse);
    }

    public int hasString(String str) {
        return 0;
    }

    public int replaceString(String oldstr, String newstr) {
        return 0;
    }

    public int replacePropDefID(int id1, int id2) {
        return 0;
    }

    public int replacePropUseID(int id1, int id2) {
        return 0;
    }

    public int maxPropRef() {
        return 0;
    }
    
    public int insertCodeAfter(Code c1, Code c2) {
        return 0;
    }
    
    public int insertCodeBefore(Code c1, Code c2) {
        return 0;
    }

    public int prependPropCode(int prop, Code c) {
        return 0;
    }
    
    public int replaceCode(Code c1, Code c2, String rep, int index, int count) {
        return 0;
    }

    public void findTextMatches(java.util.List<String> matches, String text, boolean set) {

    }

    void write(ByteBuffer bb, int baseaddr, CryptSettings crypt) {
        this.writeSize = data.length+(orgSize-orgUse);
        this.writeUse = data.length;
        this.writeAddr = baseaddr + bb.position();

        bb.put((byte)type);
        bb.putShort((short)id);
        bb.putShort((short)(data.length+(orgSize-orgUse)));
        bb.putShort((short)data.length);

        ByteBuffer bb2 = ByteBuffer.wrap(data);
        bb2.order(ByteOrder.LITTLE_ENDIAN);
        crypt.crypt(bb2, data.length, false);
        bb.put(data, 0, orgUse);
    }

    int getSize() {
        int size = 7;//type, id, size, use
        size += data.length;
        return size;
    }
}
