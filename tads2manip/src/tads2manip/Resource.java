package tads2manip;

import java.io.IOException;
import java.nio.ByteBuffer;
import java.util.ArrayList;

public class Resource {
    public String name;
    public ArrayList<Obj> objects;
    public byte[] data;
    ArrayList<HtmlRes> htmlres = null;
    ArrayList<Vocab> voc = null;

    public Resource(String name, ByteBuffer bb, int size, CryptSettings crypt) throws IOException {
        this.name = name;
        data = null;
        objects = null;
        int relTarget = bb.position() + size;

        if(name.equalsIgnoreCase("obj")) {
            objects = new ArrayList<Obj>();
            TADS2patcherHelper.objs = objects;
            TADS2ResExtHelper.objs = objects;
            antiscads2.objs = objects;
            while(bb.position() < relTarget) {
                int objtype = bb.get();
                int objid = bb.getShort();
                int objsize = bb.getShort();
                int objuse = bb.getShort();
                //System.out.println("" + objtype + ", " + Integer.toHexString(objid) + ", " + Integer.toHexString(objsize) + ", " + Integer.toHexString(objuse));
                Obj object;
                if(objtype == 1) {
                    object = new ObjFunction(objtype, objid, objsize, objuse);
                }
                else if(objtype == 2) {
                    object = new ObjObject(objtype, objid, objsize, objuse);
                }
                else {
                    object = new ObjShrug(objtype, objid, objsize, objuse);
                }
                object.parseObj(bb, crypt);
                objects.add(object);
            }
        } else if(name.equalsIgnoreCase("htmlres")) {
            int numRes = bb.getInt();
            bb.getInt();//Offset of data region relative to before numRes
            htmlres = new ArrayList<HtmlRes>(numRes);
            for(int x = 0; x < numRes; x++) {
                int off = bb.getInt();
                int siz = bb.getInt();
                int pathlength = bb.getShort();
                StringBuilder builder = new StringBuilder();
                for(int y = 0; y < pathlength; y++) {
                    builder.append((char)bb.get());
                }
                String path = builder.toString();
                htmlres.add(new HtmlRes(off, siz, path));
            }

            int totalsize = 0;
            for(int x = 0; x < numRes; x++) {
                htmlres.get(x).readData(bb);
                totalsize += htmlres.get(x).size;
            }
            bb.position(bb.position() + totalsize);
        } else if(name.equalsIgnoreCase("voc")) {
            voc = new ArrayList<Vocab>();
            while(bb.position() < relTarget) {
                short vsize1 = bb.getShort();
                short vsize2 = bb.getShort();
                short vtype = bb.getShort();
                short vid = bb.getShort();
                short unknownVal = bb.getShort();
                int strPos = bb.position();
                
                crypt.crypt(bb, vsize1+vsize2, true);
                
                StringBuilder builder = new StringBuilder();
                for(int y = 0; y < vsize1; y++) {
                    builder.append((char)bb.get());
                }
                String word1 = builder.toString();
                builder = new StringBuilder();
                for(int y = 0; y < vsize2; y++) {
                    builder.append((char)bb.get());
                }
                String word2 = builder.toString();
                
                bb.position(strPos);
                crypt.crypt(bb, vsize1+vsize2, false);
                
                voc.add(new Vocab(word1, word2, vtype, vid, unknownVal));
            }
        } else if(name.equalsIgnoreCase("fst")) {
            //do nothing
        } else if(bb.position() < relTarget) {
            //System.out.println(name + ", " + Integer.toHexString(size));
            data = new byte[size];
            bb.get(data, 0, size);
            if(name == "XSI") {
                crypt.seed = data[0];
                crypt.inc = data[1];
                if(crypt.seed < 0) {
                    crypt.seed += 256;
                }
                if(crypt.inc < 0) {
                    crypt.inc += 256;
                }
            }
        } else {
            System.out.println(name + ", " + Integer.toHexString(size));
        }
    }

    public void fixAddresses() {
        if(objects != null) {
            for(int x = 0; x < objects.size(); x++) {
                objects.get(x).fixAddresses();
            }
        }
        if(htmlres != null) {
            int pos = 0;
            for(int x = 0; x < htmlres.size(); x++) {
                htmlres.get(x).offset = pos;
                pos += htmlres.get(x).size;
            }
        }
        if(voc != null) {
            //do nothing?
        }
    }

    public void write(ByteBuffer bb, int baseaddr, int nextaddr, CryptSettings crypt) {
        bb.put((byte)name.length());
        for(int x = 0; x < name.length(); x++) {
            bb.put((byte)name.charAt(x));
        }
        bb.putInt(nextaddr);
        if(objects != null) {
            for(int x = 0; x < objects.size(); x++) {
                objects.get(x).write(bb, baseaddr, crypt);
            }
        } else if(htmlres != null) {
            bb.putInt(htmlres.size());
            int tempsize = 8;
            for(int x = 0; x < htmlres.size(); x++) {
                tempsize += htmlres.get(x).path.length();
                tempsize += 10;
            }
            bb.putInt(tempsize);
            for(int x = 0; x < htmlres.size(); x++) {
                bb.putInt(htmlres.get(x).offset);
                bb.putInt(htmlres.get(x).size);
                bb.putShort((short)htmlres.get(x).path.length());
                for(int y = 0; y < htmlres.get(x).path.length(); y++) {
                    bb.put((byte)htmlres.get(x).path.charAt(y));
                }
            }
            for(int x = 0; x < htmlres.size(); x++) {
                if(htmlres.get(x).size > 0) {
                    bb.put(htmlres.get(x).data, 0, htmlres.get(x).size);
                }
            }
        } else if(voc != null) {
            for(int x = 0; x < voc.size(); x++) {
                bb.putShort((short)voc.get(x).word1.length());
                bb.putShort((short)voc.get(x).word2.length());
                bb.putShort(voc.get(x).propNum);
                bb.putShort(voc.get(x).objNum);
                bb.putShort(voc.get(x).classFlg);
                int strPos = bb.position();
                for(int y = 0; y < voc.get(x).word1.length(); y++) {
                    bb.put((byte)voc.get(x).word1.charAt(y));
                }
                for(int y = 0; y < voc.get(x).word2.length(); y++) {
                    bb.put((byte)voc.get(x).word2.charAt(y));
                }
                bb.position(strPos);
                crypt.crypt(bb, voc.get(x).word1.length()+voc.get(x).word2.length(), false);
            }
        } else if(name.equalsIgnoreCase("fst")) {
            for(int count = 0; count < TADS2patcherHelper.objs.size(); count++) {
                int otype = TADS2patcherHelper.objs.get(count).type;
                int oid = TADS2patcherHelper.objs.get(count).id;
                int osize = TADS2patcherHelper.objs.get(count).writeSize;
                int ouse = TADS2patcherHelper.objs.get(count).writeUse;
                int oaddr = TADS2patcherHelper.objs.get(count).writeAddr;
                bb.put((byte)otype);
                bb.putShort((short)oid);
                bb.putShort((short)osize);
                bb.putShort((short)ouse);
                bb.putInt(oaddr);
            }
        } else if (data != null) {
            bb.put(data, 0, data.length);
        }
    }

    public int getSize() {
        int size = 1 + name.length() + 4;
        if(objects != null) {
            for(int x = 0; x < objects.size(); x++) {
                size += objects.get(x).getSize();
            }
        }

        if(htmlres != null) {
            size += 8;
            for(int x = 0; x < htmlres.size(); x++) {
                size += htmlres.get(x).size;
                size += htmlres.get(x).path.length();
                size += 10;//offset, size, 2byte pathlength
            }
        }
        
        if(voc != null) {
            for(int x = 0; x < voc.size(); x++) {
                size += 10;//len1, len2, propNum, objNum, classFlag
                size += voc.get(x).word1.length();
                size += voc.get(x).word2.length();
            }
        }
        
        if(name.toLowerCase().equals("fst")) {
            size += 11 * TADS2patcherHelper.objs.size();
        }
        
        if(data != null) {
            size += data.length;
        }
        return size;
    }
}
