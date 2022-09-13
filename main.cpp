#include <systemc.h>

SC_MODULE(Adder)          // module (class) declaration
{
  sc_in<int> a, b;        // ports
  sc_out<int> sum;

  void do_add()           // process
  {
    sum = a + b;
  }

  SC_CTOR(Adder)          // constructor
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
        }
    }

    SC_CTOR(Rom)
    {
        SC_METHOD(fetch);
        sensitive << address << data;

        memory[0] = 100;
        memory[1] = 200;
        memory[2] = 300;
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
            }
            else
            {
                data = memory[address - memory_offset]; 
            }
        }
    };

    SC_CTOR(Ram)
    {
        SC_METHOD(read_write);
        sensitive << address << data << write_enable;

        memory[0] = 100;
        memory[1] = 200;
        memory[2] = 300;
    }

};

SC_MODULE(Simpletron)
{
    sc_in<bool> clk;
    sc_out<int> address;
    sc_inout<int> data;
    sc_out<bool> ram_write_enable; // WE = 0 -> Read, WE = 1 -> Write

    int accumulator = 0;
    int instruction_pointer = 0;

    int opcode;
    int operand_lo;
    int operand_hi;

    enum State {FETCH_1, FETCH_2, FETCH_3, EXEC};
    State state = FETCH_1;
    
    enum OpCodes {LOAD = 0x20, STORE = 0x21};

    void load()
    {
	    accumulator = data;
        ram_write_enable = false;
    }
    
    void store()
    {
        data = accumulator;
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
            switch(state)
            {
                case FETCH_1:
                    std::cout << "fetch 1 ip " << instruction_pointer << std::endl;
                    address = instruction_pointer;
                    opcode = data;
                    instruction_pointer++;
                    state = FETCH_2;
                    std::cout << "FETCH_1 address: " << address << std::endl; 
                    std::cout << "fetch 1 ip " << instruction_pointer << std::endl;
                    break;
                case FETCH_2:
                    std::cout << "fetch 2 ip " << instruction_pointer << std::endl;
                    address = instruction_pointer;
                    operand_lo = data;
                    instruction_pointer++;
                    state = FETCH_3;
                    std::cout << "FETCH_2 address: " << address << std::endl; 
                    std::cout << "fetch 2 ip " << instruction_pointer << std::endl;
                    break;
                case FETCH_3:
                    std::cout << "fetch 3 ip " << instruction_pointer << std::endl;
                    address = instruction_pointer;
                    operand_hi = data;
                    instruction_pointer++;
                    state = EXEC;
                    std::cout << "FETCH_3 address: " << address << std::endl; 
                    std::cout << "fetch 3 ip " << instruction_pointer << std::endl;
                    break;
                case EXEC:
                    execute_op();
                    state = FETCH_1;
                    std::cout << "EXEC" << std::endl; 

            }
            std::cout << sc_time_stamp() << std::endl;
            wait();
        }
    }
    
    SC_CTOR(Simpletron)
    {
        SC_THREAD(run);
        sensitive << clk.neg(); 
    }

};

void test_simpletron()
{
    sc_clock clk("clock", 10, SC_US);
    sc_signal<int> address;
    sc_signal<int> data;
    sc_signal<bool> ram_write_enable; // WE = 0 -> Read, WE = 1 -> Write

    Simpletron simpletron("simpletron1");
    simpletron.clk(clk);
    simpletron.address(address);
    simpletron.data(data);
    simpletron.ram_write_enable(ram_write_enable);
    
    sc_core::sc_start(100, sc_core::SC_US);
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
   
//    test_ram();
    test_simpletron();
    return 0;

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
}