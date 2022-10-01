#include <systemc.h>
#include <vector>

std::vector<unsigned> prog1 = {
    0x10, 0x60, 0x99, // READ      0x0099 - Read N1 from console to address 0x0099
    0x10, 0x60, 0x98, // READ      0x0098 - Read N2 from console to address 0x0098
    0x20, 0x60, 0x99, // LOAD      0x0099 - Load N1 to acc
    0x31, 0x60, 0x98, // SUB       0x0098 - Subtract N2 from N1
    0x41, 0x00, 0x15, // BRANCHNEG 0x0008 - If N2 > N1, goto 0x0015
    0x11, 0x60, 0x99, // WRITE     0x0099 - Write N1 to console
    0x40, 0x00, 0x18, // BRANCH    0x0009 - Go to end
    0x11, 0x60, 0x98, // WRITE     0x0098 - Write N2 to console
    0x43, 0x00, 0x00, // HALT
};

SC_MODULE(Adder)          // module (class) declaration
{
    sc_in<int> a, b;      // ports
    sc_out<int> sum;

    void do_add()         // process
    {
    sum = a + b;
    }

    SC_CTOR(Adder)        // constructor
    {
    SC_METHOD(do_add);    // register do_add to kernel
    sensitive << a << b;  // sensitivity list of do_add
    }
};

SC_MODULE(Rom)
{
    sc_in<int> address;
    sc_out<int> data;

    int memory[0x6000];

    void fetch()
    {
        // Faz papel do CE
        if (address >= 0 && address < 0x6000)
        {
            data = memory[address];
            std::cout << "Fetching: " << std::hex << memory[address] << std::endl;
        }
    }

    void load_prog(std::vector<unsigned> prog)
    {
        for (unsigned i = 0; i < prog.size(); i++)
        {
            memory[i] = prog[i];
        }
    }

    SC_CTOR(Rom)
    {
        SC_METHOD(fetch);
        sensitive << address;

        /* READ 0x6000 */
//        memory[0] = 0x10;
//        memory[1] = 0x60;
//        memory[2] = 0x00;
//
//        /* WRITE 0x6000 */
//        memory[3] = 0x11;
//        memory[4] = 0x60;
//        memory[5] = 0x00;
//
//        /* LOAD 0x6002 */
//        memory[6] = 0x20;
//        memory[7] = 0x60;
//        memory[8] = 0x00;
//
//        /* ADD 0x3000 */
//        memory[9] = 0x30;
//        memory[10] = 0x30;
//        memory[11] = 0x00;
//
//        /* STORE 0x6000 */
//        memory[12] = 0x21;
//        memory[13] = 0x60;
//        memory[14] = 0x00;
//
//        /* BRANCH 0x0000 */
//        memory[15] = 0x40;
//        memory[16] = 0x00;
//        memory[17] = 0x00;
//
//        memory[0x3000] = 0x40;
//        memory[0x3001] = 0x50;
//        memory[0x3002] = 0x60;
    }

};

SC_MODULE(Ram)
{
    static const int memory_offset = 0x6000;
    sc_in<int> address;
    sc_inout<int> data;
    sc_in<bool> write_enable; // WE = 0 -> Read, WE = 1 -> Write

    int memory[0x6000];

    void read_write()
    {
        if (address >= 0x6000 && address < 0xC000)
        {
            if (write_enable)
            {
                memory[address - memory_offset] = data;
                std::cout << "Writing on RAM" << std::endl;
            }
            else
            {
                data = memory[address - memory_offset];
                std::cout << "Reading RAM" << std::endl; 
            }
        }
    };

    SC_CTOR(Ram)
    {
        SC_METHOD(read_write);
        sensitive << address << write_enable;

        memory[0] = 0x2000;
        memory[1] = 0x3000;
        memory[2] = 0x4000;
    }

};

SC_MODULE(Simpletron)
{
    sc_in<bool> clk;
    sc_out<int> address;
    sc_inout<int> data;
    sc_out<bool> ram_write_enable; // WE = 0 -> Read, WE = 1 -> Write
    sc_out<int> acc_debug;

    int accumulator = 0;
    int fetch_pointer = 0;
    bool initialized = false;
    bool executing = true;

    int opcode;
    int operand_lo;
    int operand_hi;

    enum State {FETCH_1, FETCH_2, DECODE, EXEC, NEXT_OP};
    State state = FETCH_1;
    
    enum OpCodes {READ = 0x10,
                  WRITE = 0x11,
                  LOAD = 0x20,
                  STORE = 0x21,
                  ADD = 0x30,
                  SUB = 0x31,
                  DIV = 0x32,
                  MUL = 0x33,
                  BRANCH = 0x40,
                  BRANCHNEG = 0x41,
                  BRANCHZERO = 0x42,
                  HALT = 0x43
                 };

    void load()
    {
        std::cout << "EXECUTING LOAD" << std::endl;
        accumulator = data;
        std::cout << "Data: " << std::hex << data << std::endl;
        std::cout << "Accumulator: " << std::hex << accumulator << std::endl;
        ram_write_enable = false;
    }
    
    void store()
    {
        std::cout << "EXECUTING STORE" << std::endl;
        data = accumulator;
        std::cout << "Data: " << std::hex << data << std::endl;
        std::cout << "Accumulator: " << std::hex << accumulator << std::endl;
        ram_write_enable = true;
    }
    void halt()
    {
        std::cout << "EXECUTING HALT" << std::endl;
        int stop;
        executing = false; 
        std::cin >> stop;
    }


    void add()
    {
        std::cout << "EXECUTING ADD" << std::endl;
        accumulator += data; 
    }


    void sub()
    {
        std::cout << "EXECUTING SUB" << std::endl;
        accumulator -= data;
    }


    void div()
    {
        std::cout << "EXECUTING DIV" << std::endl;

        if (data != 0){
            accumulator /= data;
        } else {
            std::cout << "Error: Division by zero"; 
            halt();
        }
    }


    void mul()
    {
        std::cout << "EXECUTING MUL" << std::endl;
        accumulator *= data;
    }

    void branch()
    {
        std::cout << "EXECUTING BRANCH" << std::endl;

        fetch_pointer = address; 
    }


    void branchneg()
    {
        std::cout << "EXECUTING BRANCHNEG" << std::endl;

        if (accumulator < 0) {
            fetch_pointer = address;
        }
    }


    void branchzero()
    {
        std::cout << "EXECUTING BRANCHZERO" << std::endl;

        if (accumulator == 0) {
            fetch_pointer = address;
        }
    }

    void write()
    {
        std::cout << "EXECUTING WRITE" << std::endl;

        std::cout << "### CONSOLE: " << std::hex << data << std::endl;
    }


    void read()
    { 
        std::cout << "EXECUTING READ" << std::endl;

        int read_data;
        std::cout << " ### CONSOLE: ";
        std::cin >> read_data;

        data = read_data;
        ram_write_enable = true;
    }



    void execute_op()
    {
        switch(opcode)
        {
            case READ:
                read();
                break;
            case WRITE:
                write();
                break;
            case LOAD:
                load();
                break;
            case STORE:
                store();
                break;
            case ADD:
                add();
                break;
            case SUB:
                sub();
                break;
            case DIV: 
                div();
                break;
            case MUL: 
                mul();
                break;
            case BRANCH:
                branch();
                break;
            case BRANCHNEG:
                branchneg();
                break;
            case BRANCHZERO:
                branchzero();
                break;
            case HALT:
                halt();
                break;

        }
        
    }
    
    void run()
    {
        
        while(true){
           
            std::cout << "---------------------------------------------------------------------" << std::endl;
            std::cout << "Iteration begin, address: " << address << std::endl;
            std::cout << "Clock: " << clk << std::endl; 
        
            if (!initialized){
                initialized = true;
            } 
            else if (executing){
                switch(state)
                {
                    case FETCH_1:
                        std::cout << "FETCH_1 ip signal: " << address << std::endl; 
                        std::cout << "Data: " << std::hex << data << std::endl;
                        std::cout << "Accumulator: " << std::hex << accumulator << std::endl; 
                        opcode = data;
                        fetch_pointer++;
                        address = fetch_pointer;
                        state = FETCH_2;
                        break;
                    case FETCH_2:
                        std::cout << "FETCH_2 address: " << address << std::endl; 
                        std::cout << "Data: " << std::hex << data << std::endl; 
                        std::cout << "Accumulator: " << std::hex << accumulator << std::endl;
                        operand_hi = data;
                        fetch_pointer++;
                        address = fetch_pointer;
                        state = DECODE;
                        break;
                    case DECODE:
                        std::cout << "FETCH_3 address: " << address << std::endl; 
                        std::cout << "Data: " << std::hex << data << std::endl;
                        std::cout << "Accumulator: " << std::hex << accumulator << std::endl;
                        operand_lo = data;
                        fetch_pointer++;
                        address = (operand_hi << 8) | operand_lo; 
                        state = EXEC;
                        break;
                   case EXEC:
                        std::cout << "EXEC" << std::endl; 
                        std::cout << "Data: " << std::hex << data << std::endl;
                        std::cout << "Accumulator: " << std::hex << accumulator << std::endl;
                        execute_op();
                        state = NEXT_OP;
                        break;
                    case NEXT_OP:
                        std::cout << "NEXT_OP" << std::endl; 
                        std::cout << "Data: " << std::hex << data << std::endl;
                        std::cout << "Accumulator: " << std::hex << accumulator << std::endl;
                        address = fetch_pointer;
                        ram_write_enable = false;
                        state = FETCH_1;
                }
                acc_debug = accumulator;
            }
            

            std::cout << sc_time_stamp() << std::endl;
            wait();
        }
    }
    
    SC_CTOR(Simpletron)
    {
        SC_THREAD(run);
        sensitive << clk.pos(); 
    }

};

struct mystr {
    int a;
    int b;
};

/*
void test_simpletron()
{
    sc_clock clk("clock", 10, SC_US, false);
    sc_signal<int> address;
    sc_signal<int> data;
    sc_signal<bool> ram_write_enable; // WE = 0 -> Read, WE = 1 -> Write

    std::cout << "Address initial value: " << address << std::endl;

    Simpletron simpletron("simpletron1");
    simpletron.clk(clk);
    simpletron.address(address);
    simpletron.data(data);
    simpletron.ram_write_enable(ram_write_enable);
    
    sc_core::sc_start(100, sc_core::SC_US);
}
*/
void test_simpletron_rom_ram()
{
    sc_clock clk("clock", 10, sc_core::SC_US, 0.5, 10, sc_core::SC_US);
    sc_signal<int> address;
    sc_signal<int, SC_MANY_WRITERS> data;
    sc_signal<bool> ram_write_enable; // WE = 0 -> Read, WE = 1 -> Write
    sc_signal<int> acc_debug;
    
    // Open VCD file
    sc_trace_file *wf = sc_create_vcd_trace_file("simpletron");

    // Dump the desired signals
    sc_trace(wf, clk, "clock");
    sc_trace(wf, address, "address");
    sc_trace(wf, data, "data");
    sc_trace(wf, ram_write_enable, "ram_we");
    sc_trace(wf, acc_debug, "accumulator_debug");

    Simpletron simpletron("simpletron1");
    simpletron.clk(clk);
    simpletron.address(address);
    simpletron.data(data);
    simpletron.ram_write_enable(ram_write_enable);
    simpletron.acc_debug(acc_debug);

    Rom rom("rom1");
    rom.address(address);
    rom.data(data);

    rom.load_prog(prog1);

    Ram ram("ram1");
    ram.address(address);
    ram.data(data);
    ram.write_enable(ram_write_enable);
    

    sc_core::sc_start(500, sc_core::SC_US);
    sc_close_vcd_trace_file(wf);
}

void test_ram()
{
    sc_signal<int> address;
    sc_signal<int> data;
    sc_signal<bool> write_enable;

    Ram ram("ram");
    ram.address(address);
    ram.data(data);
    ram.write_enable(write_enable);

    write_enable = true;
    address = 0x6000;
    data = 100;
    sc_core::sc_start(1, sc_core::SC_MS);

    std::cout << "Writing " << data << " in address " << address << std::endl;

    for(int i = 0; i < 10; i++)
    {
        address = address + 1;
        data = data + 1;
        sc_core::sc_start(1, sc_core::SC_MS);
        std::cout << "Writing " << data << " in address " << address << std::endl;
    }

    write_enable = false;
    address = 0x6000;
    sc_core::sc_start(1, sc_core::SC_MS);

    for(int i = 0; i < 10; i++)
    {
        address = address + 1;
        sc_core::sc_start(1, sc_core::SC_MS);
        std::cout << "Reading " << data << " from address " << address << std::endl;
    }
}

int sc_main(int argc, char* argv[]) {
   
    //test_ram();
    test_simpletron_rom_ram();
    return 0;


    /*
    sc_signal<int> a, b;
    sc_signal<int> sum;

    sc_signal<int> address;
    sc_signal<int> data;

    Adder adder("adder1");
    adder.a(data); 
    adder.b(b); 
    adder.sum(sum);

    Rom rom("rom1");
    rom.address(address);
    rom.data(data);

    address = 0x0000;
    data = 0x0000;

    sc_core::sc_start(0, sc_core::SC_MS);
    std::cout << "a: " << a << " b: " << b << " sum: " << sum << std::endl;
    std::cout << "address: " << address << " data: " << data << std::endl; 

    a = 1;
    b = 1;

    address = 0x0001;

    sc_core::sc_start(1, sc_core::SC_MS);
    std::cout << "a: " << a << " b: " << b << " sum: " << sum << std::endl;
    std::cout << "address: " << address << " data: " << data << std::endl; 

    address = 0x0002;

    sc_core::sc_start(1, sc_core::SC_MS);
    std::cout << "a: " << a << " b: " << b << " sum: " << sum << std::endl;
    std::cout << "address: " << address << " data: " << data << std::endl; 

    
    return 0;
    */
}
