#pragma once
#include <systemc.h>
#include "mux.h"

SC_MODULE(Rom)
{
    sc_in<unsigned short> address;
    sc_out<unsigned short> data;
    sc_in<bool> clk;
    sc_in<bool> ce;

    static constexpr unsigned short base_address = MemoryMux::rom_addr;
    static constexpr unsigned short size = MemoryMux::rom_size;

    unsigned short memory[size];

    void fetch()
    {
        if (ce) {
            const unsigned short memory_address = address - base_address;
            std::cout << "[ROM]: read address = " << memory_address <<  std::endl;

            if (memory_address < size)
            {
                std::cout << "[ROM]: reading " << memory[memory_address] << " from " << memory_address << std::endl;
                data = memory[memory_address];
            }
        }
    }

    void load_prog(std::vector<unsigned short> prog)
    {
        for (unsigned short i = 0; i < prog.size(); i++)
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

