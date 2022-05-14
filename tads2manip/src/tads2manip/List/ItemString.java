package tads2manip.List;

import java.nio.ByteBuffer;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class ItemString extends ListItem {

    public String string;
    boolean includeType;

    public ItemString(int type, boolean writeType) {
        this.type = type;
        includeType = writeType;
    }

    @Override
    public void parse(ByteBuffer bb) {
        int size = bb.getShort();
        StringBuilder sb = new StringBuilder(size-2);
        for(int x = 2; x < size; x++) {
            sb.append((char)bb.get());
        }
        string = sb.toString();
    }

    public void findTextMatches(java.util.List<String> matches, String text, boolean set) {
        Pattern pattern = Pattern.compile(text);
        Matcher matcher = pattern.matcher(string);
        while(matcher.find()) {
            if((!set) || !matches.contains(matcher.group())) {
                matches.add(matcher.group());
            }
        }
    }

    public int hasString(String str) {
        if(string.indexOf(str) >= 0) {
            return 1;
        }
        else {
            return 0;
        }
    }
    public int replaceString(String oldstr, String newstr) {
        String modString = string.replaceAll(oldstr, newstr);

        if(!string.equals(modString)) {
            string = modString;
            return 1;
        }

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
        ItemString istring = (ItemString) item;
        return istring.string.equals(string);
    }

    @Override
    public void write(ByteBuffer bb) {
        if(includeType) {
            bb.put((byte)type);
        }
        bb.putShort((short)(string.length()+2));
        for(int x = 0; x < string.length(); x++) {
            bb.put((byte)string.charAt(x));
        }
    }

    @Override
    public int getSize() {
        if(includeType) {
            return 1 + 2 + string.length();
        }
        else  {
            return 2 + string.length();
        }
    }
}
