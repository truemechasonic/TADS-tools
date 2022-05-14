package tads2manip;

import tads2manip.List.ItemString;
import tads2manip.List.List;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.util.ArrayList;

public class Instruction {
    public int opcode;
    public byte[] data;

    public int addr;

    public ItemString string = null;
    public List list = null;
    public Instruction jmptarget = null;

    public SwitchCode switchCode = null;

    public static boolean suppressErrors = false;

    public Instruction(ByteBuffer bb, int addr, int functionOffset, int boundary) {
        opcode = bb.get();
        if(opcode < 0) opcode += 256;
        this.addr = addr;

        //System.err.print(" "+Integer.toHexString(opcode));
        data = null;

        short ssize;
        StringBuilder sb;
        switch (opcode) {
            case 0x0:
                break;
            case 0x1://pushnum(int); pushes a 4byte onto stack
                data = new byte[4];
                bb.get(data, 0, 4);
                break;
            case 0x2://pushobj(short); pushes object ID onto stack
                data = new byte[2];
                bb.get(data, 0, 2);
                break;
            case 0x3://neg; pop number, multiply by -1, push
                break;
            case 0x4://not; pop number, complement, push
                break;
            case 0x5://add; pop twice, add together, push
                break;
            case 0x6://sub; pop twice, subtract, push
                break;
            case 0x7://mul; pop twice, multiply, push
                break;
            case 0x8://div; pop twice, divide, push
                break;
            case 0x9://and TODO; strange behavior on lists?
                break;
            case 0xa://or TODO
                break;
            case 0xb://eq; equality test
                break;
            case 0xc://ne; inequality test
                break;
            case 0xd://gt; greater than
                break;
            case 0xe://ge; greater/equal than
                break;
            case 0xf://lt; less than
                break;
            case 0x10://le; less/equal than
                break;
            case 0x11://call(byte argc, short func); pops argc args, calls func
                data = new byte[3];
                bb.get(data, 0, 3);
                break;
            case 0x12://getp(byte argc, short prop); pops arcg args, calls prop
                data = new byte[3];
                bb.get(data, 0, 3);
                break;
            /*case 0x13://getpdata(short); (TODO)
                data = new byte[2];
                bb.get(data, 0, 2);
                break;*/
            case 0x14://getlcl(signed? short); get local variable, push its value?
                data = new byte[2];
                bb.get(data, 0, 2);
                break;
            /*case 0x15://ptrgetpdata; (TODO)

                break;*/
            case 0x16://return; return without value
                data = new byte[2];
                bb.get(data, 0, 2);
                break;
            case 0x17://retval; return top value of stack, or nil/false if empty
                data = new byte[2];
                bb.get(data, 0, 2);
                break;
            case 0x18://enter(signed? short); "enter a function"
                data = new byte[2];
                bb.get(data, 0, 2);
                break;
            case 0x19://discard; "discard top of stack"
                break;
            case 0x1a://jmp(signed? short); jumps to relative address
                data = new byte[2];
                bb.get(data, 0, 2);
                break;
            case 0x1b://jf(signed short); jump to reladdr if false
                data = new byte[2];
                bb.get(data, 0, 2);
                break;
            case 0x1c://pushself; pushes parent object
                break;
            case 0x1d://say(string); print out double-quote string
                string = new ItemString(Prop.DSTRING, false);
                string.parse(bb);
                break;
            case 0x1e://builtin(byte, short); calls builtin function
                data = new byte[3];
                bb.get(data, 0, 3);
                break;
            case 0x1f://pushstr(string); push a string
                string = new ItemString(Prop.DSTRING, false);
                string.parse(bb);
                break;
            case 0x20://pushlst(list) //TODO check correctness:
                list = new List(true);
                list.parse(bb);
                break;
            case 0x21://pushnil
                break;
            case 0x22://pushtrue
                break;
            case 0x23://pushfn(short); push function addresss
                data = new byte[2];
                bb.get(data, 0, 2);
                break;
            case 0x24://getpselfdata; push self-local data-property (not supported htmltads?)
                data = new byte[2];
                bb.get(data, 0, 2);
                break;
            /*case 0x25://this opcode is nonexistant?
                break;*/
            case 0x26://ptrcall(byte argc); call function at stack-top
                data = new byte[1];
                bb.get(data, 0, 1);
                break;
            case 0x27://ptrinh(byte argc); "inherit pointer to property (stack=prop)"
                data = new byte[1];
                bb.get(data, 0, 1);
                break;
            case 0x28://ptrgetp(byte argc); get prop by ptr; get obj,prop from stack
                data = new byte[1];
                bb.get(data, 0, 1);
                break;
            case 0x29://pass(short prop); "pass to inherited handler"
                data = new byte[2];
                bb.get(data, 0, 2);
                break;
            case 0x2a://exit; "exit turn, but continue with fuses/daemons"
                break;
            case 0x2b://abort; "abort turn, skipping fuses/daemons"
                break;
            case 0x2c://askdo; "ask for a direct object"
                break;
            case 0x2d://askio; "ask for indirect object and set preposition"
                data = new byte[2];
                bb.get(data, 0, 2);
                break;
            case 0x2e://expinh(byte argc, short prop, short obj); "inherited <superclass>.<property>"
                data = new byte[5];
                bb.get(data, 0, 5);
                break;
            case 0x2f://expinhptr(byte argc, short obj); "inherited <superclass>.<prop-pointer>"
                data = new byte[3];
                bb.get(data, 0, 3);
                break;
            case 0x30://calld(byte, short); "call function and discard value"
                data = new byte[3];
                bb.get(data, 0, 3);
                break;
            case 0x31://getpd(byte, short); call prop and discard value
                data = new byte[3];
                bb.get(data, 0, 3);
                break;
            case 0x32://builtind(byte, short); call builtin function, discard value
                data = new byte[3];
                bb.get(data, 0, 3);
                break;
            case 0x33://je(short); jump if equal
                data = new byte[2];
                bb.get(data, 0, 2);
                break;
            case 0x34://jne
                data = new byte[2];
                bb.get(data, 0, 2);
                break;
            case 0x35://jgt
                data = new byte[2];
                bb.get(data, 0, 2);
                break;
            case 0x36://jge
                data = new byte[2];
                bb.get(data, 0, 2);
                break;
            case 0x37://jlt
                data = new byte[2];
                bb.get(data, 0, 2);
                break;
            case 0x38://jle
                data = new byte[2];
                bb.get(data, 0, 2);
                break;
            case 0x39://jnand
                data = new byte[2];
                bb.get(data, 0, 2);
                break;
            case 0x3a://jnor
                data = new byte[2];
                bb.get(data, 0, 2);
                break;
            case 0x3b://jt(short); jump if true
                data = new byte[2];
                bb.get(data, 0, 2);
                break;
            case 0x3c://getpself(byte, short); push self-local prop
                data = new byte[3];
                bb.get(data, 0, 3);
                break;
            case 0x3d://getpslfd; getpself and discard value
                data = new byte[3];
                bb.get(data, 0, 3);
                break;
            case 0x3e://getpobj(byte argc, short obj, short prop); get property
                data = new byte[5];
                bb.get(data, 0, 5);
                break;
            case 0x3f://getpobjd; getpobj and discard value
                data = new byte[5];
                bb.get(data, 0, 5);
                break;
            case 0x40://index; "get an indexed entry from a list"; pops index then list
                break;
            /*case 0x41:
                break;
            case 0x42:
                break;*/
            case 0x43://pushpn(short); push prop num
                data = new byte[2];
                bb.get(data, 0, 2);
                break;
            case 0x44://jst; "jump and save top-of-stack if true"
                data = new byte[2];
                bb.get(data, 0, 2);
                break;
            case 0x45://jsf; "jump and save top-of-stack if false"
                data = new byte[2];
                bb.get(data, 0, 2);
                break;
            case 0x46://jmpd; "discard stack and then jump unconditionally"
                data = new byte[2];
                bb.get(data, 0, 2);
                break;
            case 0x47://inherit(byte, short prop); "inherit a property from superclass"
                data = new byte[3];
                bb.get(data, 0, 3);
                break;
            case 0x48://callext(byte, signed? short); "call external function"
                data = new byte[3];
                bb.get(data, 0, 3);
                break;
            case 0x49://dbgret; "return to debugger (no stack frame leaving)"

                break;
            case 0x4a://cons(byte); "construct list from top two stack elements"
                data = new byte[2];//TODO: verify whether this is byte or short
                bb.get(data, 0, 2);
                break;
            case 0x4b://switch; "switch statement"
                /*int pos = bb.position();
                ssize = bb.getShort();
                int codeStart = bb.position();
                bb.position(pos+ssize);//find switch table
                int tableStart = bb.position();
                short numCases = bb.getShort();//System.err.println(numCases + " cases");
                for(int x = 0; x < numCases; x++) {
                    byte switchtype = bb.get();
                    switch (switchtype) {
                        case 0x1://4byte number
                            bb.getInt();
                            break;
                        case 0x20://list
                        case 0x1f://string
                            bb.position(bb.position()+bb.getShort());
                            break;
                        case 0x43://pn (prop number)
                            bb.getShort();
                            break;
                        case 0x2://object
                        case 0x23://function
                            bb.getShort();
                            break;
                        case 0x1c://self
                            break;
                        case 0x21://nil
                        case 0x22://true
                            break;
                        default:
                            System.err.println("WHY YOU BASTARD " + Integer.toHexString(switchtype));
                    }
                    bb.getShort();//jump target for case
                }
                bb.getShort();//jump target for default

                data = new byte[bb.position() - pos];
                bb.position(pos);
                bb.get(data, 0, data.length);*/
                switchCode = new SwitchCode(bb, functionOffset);

                break;
            case 0x4c://argc; "get argument count"
                break;
            case 0x4d://chkargc(byte); "check actual arguments against formal count"
                data = new byte[1];
                bb.get(data, 0, 1);
                break;
            case 0x4e://line; "line record"
                ssize = (short)bb.get();
                if(ssize < 0) ssize += 256;
                bb.position(bb.position()-1);
                data = new byte[ssize];
                bb.get(data, 0, ssize);
                break;
            case 0x4f://frame; "local variable frame record"
                ssize = bb.getShort();
                bb.position(bb.position()-2);
                data = new byte[ssize];
                bb.get(data, 0, ssize);
                break;
            case 0x50://bp; "breakpoint - replaces a line instruction"
                ssize = (short)bb.get();
                if(ssize < 0) ssize += 256;
                bb.position(bb.position()-1);
                data = new byte[ssize];
                bb.get(data, 0, ssize);
                break;
            case 0x51://getdlbcl; "get debugger local"
                data = new byte[6];
                bb.get(data, 0, 6);
                break;
            case 0x52://getpptrself; "get property pointer from self"
                data = new byte[1];
                bb.get(data, 0, 1);
                break;
            case 0x53://mod; modulo
                break;
            case 0x54://band
                break;
            case 0x55://bor
                break;
            case 0x56://xor
                break;
            case 0x57://bnot
                break;
            case 0x58://shl
                break;
            case 0x59://shr
                break;
            case 0x5a://new; "create new object"
                break;
            case 0x5b://delete; "delete object"
                break;
            default:
                boolean handled = false;

                if((opcode & 0xc0) == 0xc0) {
                    handled = true;

                    int dest = opcode & 0x3;
                    int type = opcode & 0x1c;
                    int prefix = opcode & 0x20;
                    int ext = 0;

                    //if(type == 0x1c) {//extension flag
                    //    ext = bb.get();
                    //}

                    ssize = 0;
                    if(dest == 0 || dest == 1) {
                        ssize += 2;
                    }
                    if(type == 0x1c) {
                        ssize++;
                    }
                    if(ssize > 0) {
                        data = new byte[ssize];
                        bb.get(data, 0, ssize);
                    }
                }

                if(!handled && !Instruction.suppressErrors) {
                    System.err.println("OH GOD WHY! OPCODE " + Integer.toHexString(opcode) + " @" + bb.position());
                }
                break;
        }

        if(bb.position() > boundary) {
            System.err.println("whyyy\nyyyy\nyyyy");
        }
    }

    public void findTextMatches(java.util.List<String> matches, String text, boolean set) {
        if(string != null){//say || pushstr
            string.findTextMatches(matches, text, set);
        }
        else if(opcode == 0x20) {//pushlst
            list.findTextMatches(matches, text, set);
        }
        else if(opcode == 0x4b) {//switch
            switchCode.findTextMatches(matches, text, set);
        }
    }

    public int hasString(String str) {
        if(string != null){//say || pushstr
            return string.hasString(str);
        }
        else if(opcode == 0x20) {//pushlst
            return list.hasString(str);
        }
        else if(opcode == 0x4b) {//switch
            return switchCode.hasString(str);
        }
        return 0;
    }

    public int replaceString(String oldstr, String newstr) {
        if(string != null){//say || pushstr
            return string.replaceString(oldstr, newstr);
        }
        else if(opcode == 0x20) {//pushlst
            return list.replaceString(oldstr, newstr);
        }
        else if(opcode == 0x4b) {//switch
            return switchCode.replaceString(oldstr, newstr);
        }
        return 0;
    }

    public int replacePropUseID(int id1, int id2) {
        ByteBuffer bb;
        int p;
        switch (opcode) {
            case 0x12://getp(byte argc, short prop); pops arcg args, calls prop
            case 0x31://getpd(byte, short); call prop and discard value
            case 0x3c://getpself(byte, short); push self-local prop
            case 0x3d://getpslfd; getpself and discard value
            case 0x3e://getpobj(byte argc, short obj, short prop); get property
            case 0x3f://getpobjd; getpobj and discard value
                bb = ByteBuffer.wrap(data);
                bb.order(ByteOrder.LITTLE_ENDIAN);
                bb.get();
                if(opcode == 0x3e) {
                    bb.get();
                    bb.get();
                }
                p = (int) bb.getShort();
                if(p == id1) {
                    bb.position(bb.position()-2);
                    bb.putShort((short)id2);
                    return 1;
                }
                break;
            case 0x43://pushpn(short); push prop num
                bb = ByteBuffer.wrap(data);
                bb.order(ByteOrder.LITTLE_ENDIAN);
                p = (int) bb.getShort();
                if(p == id1) {
                    bb.position(bb.position()-2);
                    bb.putShort((short)id2);
                    return 1;
                }
                break;
            case 0x24://getpselfdata(short prop); push self-local data-property
                bb = ByteBuffer.wrap(data);
                bb.order(ByteOrder.LITTLE_ENDIAN);
                p = (int) bb.getShort();
                if(p == id1) {
                    bb.position(bb.position()-2);
                    bb.putShort((short)id2);
                    return 1;
                }
                break;
            case 0xc1://assign a direct value to a prop
                bb = ByteBuffer.wrap(data);
                bb.order(ByteOrder.LITTLE_ENDIAN);
                p = (int) bb.getShort();
                if(p == id1) {
                    bb.position(bb.position()-2);
                    bb.putShort((short)id2);
                    return 1;
                }
                break;
            case 0x20://pushlst(list)
                return list.replacePropUseID(id1, id2);
            case 0x4b://switch; "switch statement"
                return switchCode.replacePropUseID(id1, id2);
        }

        return 0;
    }
    
    public int maxPropRef() {
        ByteBuffer bb;
        int p = 0;
        switch (opcode) {
            case 0x12://getp(byte argc, short prop); pops arcg args, calls prop
            case 0x31://getpd(byte, short); call prop and discard value
            case 0x3c://getpself(byte, short); push self-local prop
            case 0x3d://getpslfd; getpself and discard value
            case 0x3e://getpobj(byte argc, short obj, short prop); get property
            case 0x3f://getpobjd; getpobj and discard value
                bb = ByteBuffer.wrap(data);
                bb.order(ByteOrder.LITTLE_ENDIAN);
                bb.get();
                if(opcode == 0x3e) {
                    bb.get();
                    bb.get();
                }
                p = Math.max(p, (int) bb.getShort());
                break;
            case 0x43://pushpn(short); push prop num
                bb = ByteBuffer.wrap(data);
                bb.order(ByteOrder.LITTLE_ENDIAN);
                p = Math.max(p, (int) bb.getShort());
                break;
            case 0x24://getpselfdata(short prop); push self-local data-property
                bb = ByteBuffer.wrap(data);
                bb.order(ByteOrder.LITTLE_ENDIAN);
                p = Math.max(p, (int) bb.getShort());
                break;
            case 0xc1://assign a direct value to a prop
                bb = ByteBuffer.wrap(data);
                bb.order(ByteOrder.LITTLE_ENDIAN);
                p = Math.max(p, (int) bb.getShort());
                break;
            case 0x20://pushlst(list)
                return list.maxPropRef();
            case 0x4b://switch; "switch statement"
                return switchCode.maxPropRef();
        }

        return p;
    }
    
    public int insertCodeAfter(Code c1, Code c2) {
        if(opcode == 0x4b) {//switch
            return switchCode.insertCodeAfter(c1, c2);
        }
        return 0;
    }
    
    public int insertCodeBefore(Code c1, Code c2) {
        if(opcode == 0x4b) {//switch
            return switchCode.insertCodeBefore(c1, c2);
        }
        return 0;
    }

    public int replaceCode(Code c1, Code c2, String rep, int index, int count) {
        if(opcode == 0x4b) {//switch
            return switchCode.replaceCode(c1, c2, rep, index, count);
        }
        return 0;
    }

    public void replaceInstruction(String rep, Instruction inst) {
        opcode = inst.opcode;
        data = inst.data;
        string = inst.string;
        list = inst.list;
        if(inst.switchCode != null) {
            System.err.println("ERROR: tried to replace switch.");
        }
    }

    boolean setJmpTarget(ArrayList<ArrayList<Instruction>> insts, int offset, boolean doSwitches) {
        if(jmptarget != null) {//already set
            return true;
        }

        short ssize;
        boolean fail = true;
        switch (opcode) {
            case 0x1a://jmp(signed short); jumps to relative address
            case 0x1b://jf(signed short); jump to reladdr if false
            case 0x33://je(signed short); jump if equal
            case 0x34://jne
            case 0x35://jgt
            case 0x36://jge
            case 0x37://jlt
            case 0x38://jle
            case 0x39://jnand
            case 0x3a://jnor
            case 0x3b://jt(signed short); jump if true
            case 0x44://jst; "jump and save top-of-stack if true"
            case 0x45://jsf; "jump and save top-of-stack if false"
            case 0x46://jmpd; "discard stack and then jump unconditionally"
                ByteBuffer bb = ByteBuffer.wrap(data);
                bb.order(ByteOrder.LITTLE_ENDIAN);
                int tgt = 1 + addr + bb.getShort();

                outer499: for(int x = 0; x < insts.size(); x++) {
                    for(int y = 0; y < insts.get(x).size(); y++) {
                        if(insts.get(x).get(y).addr == tgt) {
                            jmptarget = insts.get(x).get(y);
                            data = null;
                            fail = false;
                            break outer499;
                        }
                    }
                }

                break;
            case 0x4b://switch; "switch statement"
                switchCode.setJumpTargets(insts);
            default:
                fail = false;
                break;
        }

        return fail;
    }

    void fixAddresses() {
        if(switchCode != null) {//switch
            switchCode.fixAddresses(addr + 1);
        }
    }

    public int getSize() {
        int size = 1;
        if(data != null) {
            size += data.length;
        }
        else if(jmptarget != null) {
            size += 2;
        }
        else if(switchCode != null) {
            size += switchCode.getSize();
        }
        else if(string != null) {
            size += string.getSize();
        }
        else if(list != null) {
            size += list.getSize();
        }
        return size;
    }

    void write(ByteBuffer bb) {
        bb.put((byte)opcode);
        if(jmptarget != null) {
            bb.putShort((short)(jmptarget.addr - addr - 1));
        }
        else if(switchCode != null) {
            switchCode.write(bb);
        }
        else if(data != null) {
            bb.put(data, 0, data.length);
        }
        else if(string != null) {
            string.write(bb);
        }
        else if(list != null) {
            list.write(bb);
        }
    }

    public boolean equalsInstruction(Instruction inst) {
        if(opcode != inst.opcode) {
            return false;
        }
        if(data == null ^ inst.data == null) {
            return false;
        }
        if(string == null ^ inst.string == null) {
            return false;
        }
        if(list == null ^ inst.list == null) {
            return false;
        }
        if(switchCode == null ^ inst.switchCode == null) {
            return false;
        }

        if(data != null) {
            if(data.length != inst.data.length) {
                return false;
            }
            for(int x = 0; x < data.length; x++) {
                if(data[x] != inst.data[x]) {
                    return false;
                }
            }
        }
        if(string != null) {
            if(!string.string.equals(inst.string.string)) {
                return false;
            }
        }
        if(list != null) {
            if(!list.equalsList(inst.list)) {
                return false;
            }
        }
        if(switchCode != null) {
            //TODO if(!switchCode.equalsSwitch(inst.switchCode)) {
                return false;
            //}
        }
        return true;
    }

    private Instruction(){}
    Instruction duplicate() {
        Instruction inst = new Instruction();
        inst.opcode = opcode;
        if(data != null) {
            inst.data = data;
        }
        if(string != null) {
            inst.string = string;
        }
        if(list != null) {
            inst.list = list;
        }
        if(switchCode != null) {
            System.err.println("ERROR: cannot duplicate switch code yet.");
        }

        return inst;
    }

    /*void printHex() {
        int print = opcode;
        if(print < 0) print += 256;
        if(print < 0x10) System.err.print("0");
        System.err.print(Integer.toHexString(print) + " ");
        if(data != null) {
            for(int x = 0; x < data.length; x++) {
                print = data[x];
                if(print < 0) print += 256;
                if(print < 0x10) System.err.print("0");
                System.err.print(Integer.toHexString(print) + " ");
            }
        }
        if(jmptarget != null) {
            print = jmptarget.addr - addr - 1;
            int temp = print & 255;
            if(temp < 0) temp += 256;
            System.err.print(Integer.toHexString(temp) + " ");

            print = print/256;
            if(print < 0) print += 256;
            if(print < 0x10) System.err.print("0");
            System.err.print(Integer.toHexString(print) + " ");
        }
        if(string != null) {
            //TODO
        }
        if(list != null) {
            //TODO
        }
    }*/
}
