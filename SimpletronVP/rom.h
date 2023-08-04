#include <systemc.h>

SC_MODULE(Rom)
{
    sc_in<int> address;
    sc_out<int> data;
    sc_in<bool> clk;
    sc_in<bool> ce;

    static constexpr int init_address = 0x000;
    static constexpr int end_address = 0x400;
    static constexpr int size = end_address - init_address;

    int memory[size];

    void fetch()
    {
        if (ce) {
            const unsigned int memory_address = address - init_address;
            std::cout << "[ROM]: read address = " << memory_address <<  std::endl;

            if (memory_address < size)
            {
                std::cout << "[ROM]: reading " << memory[memory_address] << " from " << memory_address << std::endl;
                data = memory[memory_address];
            }
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
        sensitive << clk.neg();

    }

};

