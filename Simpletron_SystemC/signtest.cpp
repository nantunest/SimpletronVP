#include <iostream>
#include <sstream>

int main()
{
	unsigned short num = 65527;

	if (0x8000 & num){
		std::cout << "number is negative" << std::endl;
		num -= 1;
		num = ~num;
		std::cout << "-" << std::dec << num << std::endl;


		unsigned short q = num/131;
		unsigned short r = num - (q*131);
		unsigned short fip6 = q << 6;
		fip6 |= (unsigned short)((r << 6)/131);
		unsigned short fip6n = (~fip6)+1;

		std::cout << "fip6: " << std::dec << fip6n << std::endl;
		std::cout << "num/131 = " << std::dec << num/131.0 << std::endl;
		std::cout << "before fip: " << static_cast<signed short>(num) << std::endl;
		unsigned short fp0 = (0x3F & fip6)*100/64;
		unsigned short fp3 = (0x3F & fip6)*10000/64;
		std::ostringstream aux;
		if (fp0 == 0)
			aux << fp0 << fp3;
		else
			aux << fp3;	

		std::cout << "conv fip: -" << std::dec << (fip6>>6) << "." << aux.str() << std::endl;
	}
	else
	{
		std::cout << "number is positive" << std::endl;
		std::cout << std::dec << num << std::endl;

		unsigned short q = num/131;
		unsigned short r = num - (q*131);
		unsigned short fip6 = q << 6;
		fip6 |= (unsigned short)((r << 6)/131);

		std::cout << "fip6: " << std::dec << fip6 << std::endl;
		std::cout << "num/131 = " << std::dec << num/131.0 << std::endl;
		std::cout << "before fip: " << static_cast<signed short>(num) << std::endl;
		unsigned short fp0 = (0x3F & fip6)*100/64;
		unsigned short fp3 = (0x3F & fip6)*10000/64;
		std::ostringstream aux;
		if (fp0 == 0)
			aux << fp0 << fp3;
		else
			aux << fp3;	

		std::cout << "conv fip: " << std::dec << (fip6>>6) << "." << aux.str() << std::endl;
	}
	
}
