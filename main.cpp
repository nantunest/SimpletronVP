#include <systemc.h>

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

    SC_CTOR(Rom)
    {
        SC_METHOD(fetch);
        sensitive << address;

        memory[0] = 0x20;
        memory[1] = 0x60;
        memory[2] = 0x02;

        memory[3] = 0x21;
        memory[4] = 0x60;
        memory[5] = 0x00;

        memory[6] = 0x20;
        memory[7] = 0x60;
        memory[8] = 0x01;

        memory[0x3000] = 0x40;
        memory[0x3001] = 0x50;
        memory[0x3002] = 0x60;
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

    int accumulator = 0;
    int fetch_pointer = 0;
    bool initialized = false;

    int opcode;
    int operand_lo;
    int operand_hi;

    enum State {FETCH_1, FETCH_2, FETCH_3, DECODE, EXEC, NEXT_OP};
    State state = FETCH_1;
    
    enum OpCodes {LOAD = 0x20, STORE = 0x21};

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

    void execute_op()
    {
        switch(opcode)
        {
            case LOAD:
                load();
                break;
            case STORE:
                store();
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
            else {
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
                        state = FETCH_3;
                        break;
                    case FETCH_3:
                        std::cout << "FETCH_3 address: " << address << std::endl; 
                        std::cout << "Data: " << std::hex << data << std::endl;
                        std::cout << "Accumulator: " << std::hex << accumulator << std::endl;
                        operand_lo = data;
                        fetch_pointer++;
                        state = DECODE;
                        break;
                    case DECODE:
                        std::cout << "DECODE address: " << address << std::endl; 
                        std::cout << "Data: " << std::hex << data << std::endl;
                        std::cout << "Accumulator: " << std::hex << accumulator << std::endl; 
                        address = (operand_hi << 8) | operand_lo; // quando atualizo address já executa função RAM/ROM
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
    
    // Open VCD file
    sc_trace_file *wf = sc_create_vcd_trace_file("simpletron");

    // Dump the desired signals
    sc_trace(wf, clk, "clock");
    sc_trace(wf, address, "address");
    sc_trace(wf, data, "data");
    sc_trace(wf, ram_write_enable, "ram_we");

    Simpletron simpletron("simpletron1");
    simpletron.clk(clk);
    simpletron.address(address);
    simpletron.data(data);
    simpletron.ram_write_enable(ram_write_enable);

    Rom rom("rom1");
    rom.address(address);
    rom.data(data);

    Ram ram("ram");
    ram.address(address);
    ram.data(data);
    ram.write_enable(ram_write_enable);
    

    sc_core::sc_start(180, sc_core::SC_US);
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
