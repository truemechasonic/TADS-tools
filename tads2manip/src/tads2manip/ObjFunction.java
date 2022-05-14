package tads2manip;

import java.io.IOException;
import java.nio.ByteBuffer;

public class ObjFunction extends Obj {
    Code code = null;

    public ObjFunction(int type, int id, int orgSize, int orgUse) {
        super(type,id,orgSize,orgUse);
    }

    public void parseObj(ByteBuffer bb, CryptSettings crypt) throws IOException {
        //System.err.println("fun" + this.id + " (" + this.orgUse + ") @" + (bb.position()-1) + ": ");

        crypt.crypt(bb, orgUse, true);
        code = new Code(bb, orgUse);

        //code.printHex();
    }

    public int hasString(String str) {
        return code.hasString(str);
    }

    public int replaceString(String oldstr, String newstr) {
        return code.replaceString(oldstr, newstr);
    }

    public int replacePropDefID(int id1, int id2) {
        return 0;
    }

    public int replacePropUseID(int id1, int id2) {
        return code.replacePropUseID(id1, id2);
    }

    public int maxPropRef() {
        return code.maxPropRef();
    }
    
    public int insertCodeAfter(Code c1, Code c2) {
        return code.insertCodeAfter(c1, c2);
    }
    
    public int insertCodeBefore(Code c1, Code c2) {
        return code.insertCodeBefore(c1, c2);
    }
    
    public int prependPropCode(int prop, Code c) {
        return 0;
    }
    
    public int replaceCode(Code c1, Code c2, String rep, int index, int count) {
        return code.replaceCode(c1, c2, rep, index, count);
    }

    public void findTextMatches(java.util.List<String> matches, String text, boolean set) {
        code.findTextMatches(matches, text, set);
    }

    @Override
    public void fixAddresses() {
        code.fixAddresses();
    }

    void write(ByteBuffer bb, int baseaddr, CryptSettings crypt) {
        int size = getSize() - 7;

        this.writeSize = size+(orgSize-orgUse);
        this.writeUse = size;
        this.writeAddr = baseaddr + bb.position();

        bb.put((byte)type);
        bb.putShort((short)id);
        bb.putShort((short)(size+(orgSize-orgUse)));
        bb.putShort((short)size);

        bb.mark();

        code.write(bb);

        bb.reset();
        crypt.crypt(bb, size, false);
    }

    public int getSize() {
        if(code != null) {
            return 7 + code.getSize();
        }
        else {
            return 7;
        }
    }
}
