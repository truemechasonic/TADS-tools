package tads2manip;

import tads2manip.List.List;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Prop {
    public static final int NUM = 1;
    public static final int OBJECT = 2;
    public final static int SSTRING = 3;
    public final static int BASEPTR = 4;
    public final static int NIL = 5;
    public static final int CODE = 6;
    public static final int LIST = 7;
    public static final int TRUE = 8;
    public static final int DSTRING = 9;
    public static final int FNADDR = 10;
    public static final int TPL = 11;
    //there's no 12
    public static final int PROPNUM = 13;
    public static final int DEMAND = 14;
    public static final int SYN = 15;
    public static final int REDIR = 16;
    public static final int TPL2 = 17;

    private int id;
    private int type;
    //private int size;
    private int dummy;
    byte[] body;

    String string = null;
    List list = null;
    Code code = null;

    public Prop(short id, byte type, short size, byte dummy, ByteBuffer bb) {
        this.id = id;
        this.type = type;
        this.dummy = dummy;
        if(size == 0) {
            body = null;
        }
        else {
            if(type == Prop.SSTRING || type == Prop.DSTRING) {
                StringBuilder sb = new StringBuilder(size-2);
                bb.getShort();//string size
                for(int x = 2; x < size; x++) {
                    sb.append((char)bb.get());
                }
                string = sb.toString();
                body = null;
            }
            else if(type == Prop.LIST) {
                list = new List(true);
                list.parse(bb);
            }
            else if(type == Prop.CODE) {
                //System.err.println("+" + (bb.position()-size));

                body = new byte[size];
                bb.get(body, 0, size);

                ByteBuffer temp = ByteBuffer.wrap(body);
                temp.order(ByteOrder.LITTLE_ENDIAN);
                code = new Code(temp, body.length);
                body = null;

                //System.err.println("---");
            }
            else if(size > 0) {
                body = new byte[size];
                bb.get(body, 0, size);
            }
            else {
                body = null;
            }
        }
    }

    public int getID() {
        return this.id;
    }
    
     public int getType() {
        return this.type;
    }
    
    public boolean isEqual(Prop p) {
        if(p.id != id) {
            return false;
        }
        if(p.getSize() != getSize()) {
            return false;
        }

        if(body != null) {
            if(p.body == null) {
                return false;
            }
            for(int x = 0; x < body.length; x++) {
                if(body[x] != p.body[x]) {
                    return false;
                }
            }
        }
        else if(p.body != null) {
            return false;
        }

        if(string != null || p.string != null) {
            if(string == null || p.string == null) {
                return false;
            }
            if(!p.string.equals(string)) {
                return false;
            }
        }

        if(list != null || p.list != null) {
            if(list == null || p.list == null) {
                return false;
            }
            if(list.items.size() != p.list.items.size()) {
                return false;
            }
            for(int x = 0; x < list.items.size(); x++) {
                if(!list.items.get(x).equalsItem(p.list.items.get(x))) {
                    return false;
                }
            }
        }

        if(code != null || p.code != null) {
            if(code == null || p.code == null) {
                return false;
            }
            //TODO
            //if(!code.isEqual(p.code)) {
                return false;
            //}
        }

        return true;
    }

    public void findTextMatches(java.util.List<String> matches, String text, boolean set) {
        if(type == Prop.SSTRING || type == Prop.DSTRING) {
            Pattern pattern = Pattern.compile(text);
            Matcher matcher = pattern.matcher(string);
            while(matcher.find()) {
                if((!set) || !matches.contains(matcher.group())) {
                    matches.add(matcher.group());
                }
            }
        }
        else if(type == Prop.LIST) {
            list.findTextMatches(matches, text, set);
        }
        else if(type == Prop.CODE) {
            code.findTextMatches(matches, text, set);
        }
    }

    public int hasString(String str) {
        if(type == Prop.SSTRING || type == Prop.DSTRING) {
            if(string.indexOf(str) >= 0) {
                return 1;
            }
            else {
                return 0;
            }
        }
        else if(type == Prop.LIST) {
            return list.hasString(str);
        }
        else if(type == Prop.CODE) {
            return code.hasString(str);
        }

        return 0;
    }

    public int replaceString(String oldstr, String newstr) {
        if(type == Prop.SSTRING || type == Prop.DSTRING) {
            String modString = string.replaceAll(oldstr, newstr);

            if(!string.equals(modString)) {
                string = modString;
                return 1;
            }
        }
        else if(type == Prop.LIST) {
            return list.replaceString(oldstr, newstr);
        }
        else if(type == Prop.CODE) {
            return code.replaceString(oldstr, newstr);
        }

        return 0;
    }

    public int replacePropDefID(int id1, int id2) {
        if(id == id1) {
            id = id2;
            return 1;
        }
        return 0;
    }

    public int replacePropUseID(int id1, int id2) {
        if(type == Prop.LIST) {
            return list.replacePropUseID(id1, id2);
        }
        else if(type == Prop.CODE) {
            return code.replacePropUseID(id1, id2);
        }
        return 0;
    }
    
    int maxPropRef() {
        if(type == Prop.LIST) {
            return Math.max(this.id, list.maxPropRef());
        }
        else if(type == Prop.CODE) {
            return Math.max(this.id, code.maxPropRef());
        }
        return this.id;
    }
    
    public int insertCodeAfter(Code c1, Code c2) {
        if(type == Prop.CODE) {
            return code.insertCodeAfter(c1, c2);
        }
        return 0;
    }
    
    public int insertCodeBefore(Code c1, Code c2) {
        if(type == Prop.CODE) {
            return code.insertCodeBefore(c1, c2);
        }
        return 0;
    }
    
    public int prependPropCode(int prop, Code c) {
        if(type == Prop.CODE && this.getID() == prop) {
            return code.insertCodeBefore(null, c);
        } else {
            System.err.println("SANITY CHECK FAILED in function Prop.prependPropCode.");
        }
        return 0;
    }

    public int replaceCode(Code c1, Code c2, String rep, int index, int count) {
        if(type == Prop.CODE) {
            return code.replaceCode(c1, c2, rep, index, count);
        }
        return 0;
    }

    public void fixAddresses() {
        if(code != null) {
            code.fixAddresses();
        }
    }

    public void write(ByteBuffer bb) {
        //id
        bb.putShort((short)id);

        //type
        bb.put((byte)type);

        //size
        if(body != null) {
            bb.putShort((short)body.length);
        }
        else if(string != null) {
            bb.putShort((short)(string.length()+2));
        }
        else if(list != null) {
            bb.putShort((short)list.getSize());
        }
        else if(code != null) {
            bb.putShort((short)code.getSize());
        }
        else {
            bb.putShort((short)0);
        }

        //dummy
        bb.put((byte)dummy);

        //data
        if(body != null) {
            if(body.length > 0) {
                bb.put(body, 0, body.length);
            }
        }
        else if(string != null) {
            bb.putShort((short)(string.length()+2));
            for(int x = 0; x < string.length(); x++) {
                bb.put((byte)string.charAt(x));
            }
        }
        else if(list != null) {
            list.write(bb);
        }
        else if(code != null) {
            code.write(bb);
        }
    }

    public int getSize() {
        if(body != null) {
            return 6 + body.length;
        }
        else if(string != null) {
            return 6 + 2 + string.length();
        }
        else if(list != null) {
            return 6 + list.getSize();
        }
        else if(code != null) {
            return 6 + code.getSize();
        }
        else {
            return 6;
        }
    }
}
