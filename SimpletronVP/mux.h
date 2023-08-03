SC_MODULE(MemoryMux)
{
    sc_in<int> address;
    sc_out<bool> rom_ce;
    sc_out<bool> ram_ce;

    void do_mux()
    {
        if (address < 0x400)
        {
            rom_ce = true;
            ram_ce = false;
        }
        else if (address < 0xF00)
        {
           rom_ce = false;
           ram_ce = true;
        }
        else
        {
            rom_ce = false;
            ram_ce = false;
        }
    }

    SC_CTOR(MemoryMux)
    {
        SC_METHOD(do_mux);
        sensitive << address;
    }

};