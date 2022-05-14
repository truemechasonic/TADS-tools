package tads2manip;

import java.nio.ByteBuffer;

public class CryptSettings {
    public boolean encrypted = false;
    public int seed = 0x3f;
    public int inc = 0x40;

    public void crypt(ByteBuffer bb, int size, boolean reset) {
        if(!encrypted) {
            if(!reset) {
                bb.position(bb.position() + size);
            }
            return;
        }

        int xor = seed;
        int pos = bb.position();
        bb.mark();

        for(int x = 0; x < size; x++) {
            int c = bb.get();
            bb.position(pos+x);
            c = c ^ xor;
            c = c & 255;
            bb.put((byte)c);
            xor += inc;
            if(xor >= 256) {
                xor -= 256;
            }
        }

        if(reset) {
            bb.reset();
        }
    }
}
