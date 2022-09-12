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
        if (address >= 0 && address < 0x6000)
        {
            data = memory[address];
        }
    }

    SC_CTOR(Rom)
    {
        SC_METHOD(fetch);
        sensitive << address;

        memory[0] = 100;
        memory[1] = 200;
        memory[2] = 300;
    }

};

int sc_main(int argc, char* argv[]) {
    
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
