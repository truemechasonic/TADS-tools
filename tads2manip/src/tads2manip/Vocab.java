package tads2manip;

import java.nio.ByteBuffer;

class Vocab {
    public String word1;
    public String word2;
    public short propNum;
    public short objNum;//
    public short classFlg;

    public Vocab(String word1, String word2, short propNum, short objNum, short classFlg) {
        this.word1 = word1;
        this.word2 = word2;
        this.propNum = propNum;
        this.objNum = objNum;
        this.classFlg = classFlg;
    }
}
