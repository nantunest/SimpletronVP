#include <iostream>

int main()
{
	int neg = -9;

	uint8_t hi = (0xFF00 & neg) >> 8;
	uint8_t lo = 0xFF & neg;

	unsigned short testnum = 65527;

	std::cout << std::hex << static_cast<unsigned int>(hi) << std::endl;
	std::cout << std::hex << static_cast<unsigned int>(lo) << std::endl;

	unsigned short num = 56538;
//	unsigned short num = (hi << 8) | lo;

	std::cout << std::hex << num << std::endl;

	if (0x8000 & num){
		std::cout << "number is negative" << std::endl;
		num -= 1;
		num = ~num;
		std::cout << "-" << num << std::endl;


		unsigned short q = num/131;
		unsigned short r = num - (q*131);
		unsigned short fip6 = q << 6;
		fip6 |= (unsigned short)((r << 6)/131);

		fip6 |= 0x8000;

		std::cout << "fip6: " << std::dec << fip6 << std::endl;
		std::cout << "num/131 = " << std::dec << num/131 << std::endl;
		std::cout << "before fip: " << static_cast<signed short>(num) << std::endl;
		std::cout << "conv fip: " << std::dec << (fip6>>6) << "." << ((0x3F & fip6)/64.0)*100 << std::endl;
	}
	else
		std::cout << "number is positive" << std::endl;




	
}
