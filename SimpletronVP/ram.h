#pragma once
#include <systemc.h>
#include "mux.h"

SC_MODULE(Ram)
{
    static constexpr int base_address = MemoryMux::ram_addr;
    static constexpr int size = MemoryMux::ram_size;

    sc_in<int> address;
    sc_inout<int> data;
    sc_in<bool> write_enable; // WE = 0 -> Read, WE = 1 -> Write
    sc_in<bool> clk;
    sc_in<bool> ce;

    int memory[size];

    void read_write()
    {
        if (ce) {

            const unsigned int memory_address = address - base_address;
            std::cout << "[RAM]: read address = " << memory_address <<  std::endl;

            if (memory_address - size)
            {
                if (write_enable)
                {
                    std::cout << "[RAM]: writing " << data << " to " << memory_address << std::endl;
                    memory[memory_address] = data;
                }
                else
                {
                    std::cout << "[RAM]: reading " << memory[memory_address] << " from " << memory_address << std::endl;
                    data = memory[memory_address];
                }
            }
        }
    };

    SC_CTOR(Ram)
    {
        SC_METHOD(read_write);
        sensitive << clk.neg(); 
    }

};

