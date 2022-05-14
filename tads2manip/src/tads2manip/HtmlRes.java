package tads2manip;

import java.nio.ByteBuffer;

class HtmlRes {
    public int offset;
    public int size;
    public String path;

    public byte[] data;

    public HtmlRes(int off, int siz, String path) {
        this.offset = off;
        this.size = siz;
        this.path = path;
        data = new byte[siz];
    }

    public void readData(ByteBuffer bb) {
        int orgpos = bb.position();
        bb.position(orgpos + offset);
        bb.get(data, 0, size);
        bb.position(orgpos);
    }
}
