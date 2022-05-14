package tads2manip;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.util.ArrayList;
import java.util.Collections;
import javax.swing.JButton;
import javax.swing.JTextArea;

//This file extracts resources (e.g. images) from a tads2 game

class TADS2ResExtHelper implements Runnable {
    public static ArrayList<Obj> objs = new ArrayList<Obj>();

    public String in;
    public String out;
    public JTextArea log = new JTextArea();
    public JButton btn = null;

    public static void main(String[] args) throws IOException {
        TADS2ResExtHelper p = new TADS2ResExtHelper();
        p.in = args[0];
        p.out = args[1];
        p.patch();
    }

    @Override
    public void run() {
        try {
            patch();
        } catch (IOException ex) {
            System.err.print("ERROR reading/writing files.\n");
            log.append("ERROR reading/writing files.\n");
        }
    }

    void patch() throws FileNotFoundException, IOException {
        FileInputStream is = new FileInputStream(in);

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
            if(log != null) {
                log.append("ERROR: NOT A TADS2 GAME FILE!\n");
            }
            if(btn != null) {
                btn.setEnabled(true);
            }
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

        //Write resources to disk:
        if(!(out.endsWith("/"))) {
            out = out + "/";
        }
        for(int x = 0; x < resources.size(); x++) {
            ArrayList<HtmlRes> htmlres = resources.get(x).htmlres;
            if(htmlres != null) {
                for(int y = 0; y < htmlres.size(); y++) {
                    HtmlRes hres = htmlres.get(y);
                    File tempfile = new File(out + hres.path);
                    tempfile = new File(tempfile.getParent());
                    tempfile.mkdirs();
                    FileOutputStream os = new FileOutputStream(out + hres.path, false);
                    os.write(hres.data, 0, hres.size);
                    os.close();
                }
            }
        }
        
        /*
        //Find all image uses
        //NOTE: misses images with dynamic names; only finds names inside single string
        ArrayList<String> matches = new ArrayList<String>(500);
        for(int x = 0; x < resources.size(); x++) {
            if(resources.get(x).objects != null) {
                for(int y = 0; y < resources.get(x).objects.size(); y++) {
                    resources.get(x).objects.get(y).findTextMatches(matches, "<IMG src=[^<>]*>", true);
                }
            }
        }
        for(int x = 0; x < matches.size(); x++) {
            String temp = matches.get(x);
            if(temp.startsWith("<IMG src='")) {
                temp = temp.substring(10, temp.length()-2);
            }
            else {
                temp = temp.substring(9, temp.length()-1);
            }
            matches.set(x, temp);
        }
        Collections.sort(matches);

        //Print images used in code:
        for(int x = 0; x < matches.size(); x++) {
            System.out.println(matches.get(x));
        }
        */

        System.out.println("Extraction complete.");
        log.append("Extraction complete.\n");

        if(btn != null) {
            btn.setEnabled(true);
        }
    }

    private void fixAddresses(ArrayList<Resource> resources) {
        for(int x = 0; x < resources.size(); x++) {
            resources.get(x).fixAddresses();
        }
    }
}
