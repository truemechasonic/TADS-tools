package tads2manip;

import java.io.FileInputStream;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.util.ArrayList;

public class ObjObject extends Obj {
    private int _workspace;
    private int _flags;
    private int _free;//offset of end of all props?
    private int _reset;//number of static props?
    private int _static;//offset of end of static props?
    public ArrayList<Integer> supers;
    public ArrayList<Integer> indexTable;
    
    public ObjObject(int id) {
        super(2, id, 0, 0);
        _workspace = 0;
        _flags = 0;
        _free = 0;
        _reset = 0;
        _static = 0;
        supers = new ArrayList<Integer>();
        indexTable = new ArrayList<Integer>();
    }
    
    public ObjObject(int type, int id, int orgSize, int orgUse) {
        super(type, id, orgSize, orgUse);
        supers = new ArrayList<Integer>();
        indexTable = new ArrayList<Integer>();
    }

    public int hasString(String str) {
        int count = 0;
        for(int x = 0; x < props.size(); x++) {
            count += props.get(x).hasString(str);
        }
        return count;
    }

    public int replaceString(String oldstr, String newstr) {
        int count = 0;
        for(int x = 0; x < props.size(); x++) {
            int osize = props.get(x).getSize();
            count += props.get(x).replaceString(oldstr, newstr);
            int nsize = props.get(x).getSize();
            int diff = nsize - osize;
            if(diff != 0) {
                _free += diff;
                if(x < _reset) {
                    _static += diff;
                }
            }
        }
        return count;
    }

    public int replacePropDefID(int id1, int id2) {
        int count = 0;
        for(int x = 0; x < props.size(); x++) {
            count += props.get(x).replacePropDefID(id1, id2);
        }
        return count;
    }

    public int replacePropUseID(int id1, int id2) {
        int count = 0;
        for(int x = 0; x < props.size(); x++) {
            count += props.get(x).replacePropUseID(id1, id2);
        }
        return count;
    }

    public int maxPropRef() {
        int c = 0;
        for(int x = 0; x < props.size(); x++) {
            c = Math.max(c, props.get(x).maxPropRef());
        }
        return c;
    }
    
    public int insertCodeAfter(Code c1, Code c2) {
        int count = 0;
        for(int x = 0; x < props.size(); x++) {
            int osize = props.get(x).getSize();
            count += props.get(x).insertCodeAfter(c1, c2);
            int nsize = props.get(x).getSize();
            int diff = nsize - osize;
            if(diff != 0) {
                _free += diff;
                if(x < _reset) {
                    _static += diff;
                }
            }
        }
        return count;
    }
    
    public int insertCodeBefore(Code c1, Code c2) {
        int count = 0;
        for(int x = 0; x < props.size(); x++) {
            int osize = props.get(x).getSize();
            count += props.get(x).insertCodeBefore(c1, c2);
            int nsize = props.get(x).getSize();
            int diff = nsize - osize;
            if(diff != 0) {
                _free += diff;
                if(x < _reset) {
                    _static += diff;
                }
            }
        }
        return count;
    }
    
    public int prependPropCode(int prop, Code c) {
        int count = 0;
        for(int x = 0; x < props.size(); x++) {
            if(props.get(x).getID() == prop) {
                int osize = props.get(x).getSize();
                count += props.get(x).prependPropCode(prop, c);
                int nsize = props.get(x).getSize();
                int diff = nsize - osize;
                if(diff != 0) {
                    _free += diff;
                    if(x < _reset) {
                        _static += diff;
                    }
                }
            }
        }
        return count;
    }

    public int replaceCode(Code c1, Code c2, String rep, int index, int count) {
        int c = 0;
        for(int x = 0; x < props.size(); x++) {
            int osize = props.get(x).getSize();
            c += props.get(x).replaceCode(c1, c2, rep, index, count);
            int nsize = props.get(x).getSize();
            int diff = nsize - osize;
            if(diff != 0) {
                _free += diff;
                if(x < _reset) {
                    _static += diff;
                }
            }
        }
        return c;
    }
    
    public void findTextMatches(java.util.List<String> matches, String text, boolean set) {
        for(int x = 0; x < props.size(); x++) {
            props.get(x).findTextMatches(matches, text, set);
        }
    }

    @Override
    public void addProp(Prop prop) {
        props.add(0, prop);
        if((_flags & 0x2) > 0) {
            System.err.println("PROBLEM: flags & 0x2");
        }
        int psize = prop.getSize();
        _free += psize;
        _reset++;
        _static += psize;
    }

    @Override
    public void fixAddresses() {
        for(int x = 0; x < props.size(); x++) {
            props.get(x).fixAddresses();
        }
    }

    public void parseObj(ByteBuffer bb, CryptSettings crypt) throws IOException {
        //System.err.println("obj" + this.id + " (" + this.orgUse + "): @" + (bb.position()-1));
        crypt.crypt(bb, orgUse, true);

        _workspace = bb.getShort();
        _flags = bb.getShort();
        int numSupers = bb.getShort();
        int numProps = bb.getShort();
        _free = bb.getShort();
        _reset = bb.getShort();
        _static = bb.getShort();
        //System.err.println(_workspace + ", " + _free + ", " + _static + ", " + _reset);
        for(int x = 0; x < numSupers; x++) {
            supers.add((int)bb.getShort());
        }

        if((_flags & 0x2) > 0) {
            for(int x = 0; x < numProps; x++) {
                indexTable.add((int)bb.getShort());
            }
        }

        for(int x = 0; x < numProps; x++) {
            props.add(new Prop(bb.getShort(),bb.get(),bb.getShort(),bb.get(), bb));
        }
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

        bb.putShort((short)_workspace);
        bb.putShort((short)_flags);
        bb.putShort((short)supers.size());
        bb.putShort((short)props.size());
        bb.putShort((short)_free);
        bb.putShort((short)_reset);
        bb.putShort((short)_static);

        for(int x = 0; x < supers.size(); x++) {
            bb.putShort((short)supers.get(x).intValue());
        }

        if((_flags & 0x2) > 0) {
            for(int x = 0; x < props.size(); x++) {
                bb.putShort((short)indexTable.get(x).intValue());
            }
        }

        for(int x = 0; x < props.size(); x++) {
            props.get(x).write(bb);
        }

        bb.reset();
        crypt.crypt(bb, size, false);
    }

    int getSize() {
        int size = 7;//type, id, size, use
        size += 14;//workspace, flags, numSupers, numProps, free, reset, static

        size += 2*supers.size();
        size += 2*indexTable.size();

        for(int x = 0; x < props.size(); x++) {
            size += props.get(x).getSize();
        }

        if(data != null) {
            data = null;
            System.out.println(data[12]);
        }

        return size;
    }
}
