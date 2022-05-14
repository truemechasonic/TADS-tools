package tads2manip;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintStream;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.util.ArrayList;
import java.util.Scanner;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.JButton;
import javax.swing.JTextArea;

//This program carefully corrupts the tads2 game file to make scads2 fail.
//The game should still run okay despite this corruption.

class antiscads2 {
    public static ArrayList<Obj> objs = new ArrayList<Obj>();

    public String in;
    public String out;

    public static void main(String[] args) throws IOException {
        antiscads2 p = new antiscads2();
        p.in = args[0];
        p.out = args[1];
        //p.in = sd3.gam";
        //p.out = "sd3.antiscads2.gam";
        p.patch();
    }

    void patch() throws FileNotFoundException, IOException {
        FileInputStream is = new FileInputStream(in);

        System.out.println("Reading file...");

        ArrayList<Resource> resources = new ArrayList<Resource>();
        CryptSettings crypt = new CryptSettings();
        byte[] header = new byte[48];

        is.read(header, 0, 48);
        if((header[20] & 0x8)  > 0) {
            crypt.encrypted = true;
        }

        String tempString = "";
        for(int x = 0; x < 9; x++) {
            tempString += (char)header[x];
        }
        if(!tempString.equals("TADS2 bin")) {
            System.err.println("ERROR: NOT A TADS2 GAME FILE!");
            return;
        }

        while(true) {
            int bite = is.read();
            StringBuilder builder = new StringBuilder();
            for(int x = 0; x < bite; x++) {
                builder.append((char)is.read());
            }
            String name = builder.toString();

            if(name.equals("$EOF")) {
                break;
            }

            int b1 = is.read();
            int b2 = is.read();
            int b3 = is.read();
            int b4 = is.read();
            int nextRsc = b4 * (256*256*256) + b3 * (256*256) + b2 * 256 + b1;
            int size = nextRsc - (int)is.getChannel().position();

            byte[] data = new byte[size];
            is.read(data, 0, size);

            ByteBuffer bb = ByteBuffer.wrap(data);
            bb.order(ByteOrder.LITTLE_ENDIAN);
            Resource rec = new Resource(name, bb, size, crypt);
            resources.add(rec);
        }
        is.close();

        fixAddresses(resources);

        System.out.println("Beginning modification...");

        Instruction.suppressErrors = true;
        obfuse();
        Instruction.suppressErrors = false;

        fixAddresses(resources);

        FileOutputStream os = new FileOutputStream(out, false);
        os.write(header, 0, header.length);
        for(int x = 0; x < resources.size(); x++) {
            int size = resources.get(x).getSize();
            byte[] bytes = new byte[size];

            ByteBuffer bb = ByteBuffer.wrap(bytes);
            bb.order(ByteOrder.LITTLE_ENDIAN);
            resources.get(x).write(bb, (int)os.getChannel().position(), (int)(size+os.getChannel().position()), crypt);

            os.write(bytes, 0, size);
        }

        byte[] ending = new byte[9];
        ending[0] = 4;
        ending[1] = '$';
        ending[2] = 'E';
        ending[3] = 'O';
        ending[4] = 'F';
        ending[5] = 0;
        ending[6] = 0;
        ending[7] = 0;
        ending[8] = 0;
        os.write(ending, 0, 9);
        os.close();

        System.out.println("Complete.");
    }

    private void obfuse() {
        byte[] bytes = {(byte)0x7f, (byte)0xff, (byte)0x1a};
        ByteBuffer bb = ByteBuffer.wrap(bytes);
        Prop prop = new Prop((short)0, (byte)6, (short)3, (byte)1, bb);
        for(int x = 0; x < objs.size(); x++) {
            if(objs.get(x).type == 2) {
                objs.get(x).addProp(prop);
                break;
            }
        }
    }

    private void fixAddresses(ArrayList<Resource> resources) {
        for(int x = 0; x < resources.size(); x++) {
            resources.get(x).fixAddresses();
        }
    }
}
