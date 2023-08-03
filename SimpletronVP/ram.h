SC_MODULE(Ram)
{
    static constexpr int init_address = 0x400;
    static constexpr int end_address = 0xF00;
    static constexpr int size = end_address - init_address;

    sc_in<int> address;
    sc_inout<int> data;
    sc_in<bool> write_enable; // WE = 0 -> Read, WE = 1 -> Write
    sc_in<bool> clk;
    sc_in<bool> ce;

    int memory[size];

    void read_write()
    {
        if (ce) {

            const unsigned int memory_address = address - init_address;
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

