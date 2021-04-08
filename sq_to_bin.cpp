#include <iostream>
#include <fstream>
#include <string>
#include <vector>

int main( int argc, char *argv[] )
{

    if (argc < 2){
        std::cout<<"No file specified"<<std::endl;
        return -1;
    }

    std::ifstream input_file;
    std::ofstream output_file;
    std::vector<int16_t> buffer;

    input_file.open(argv[1], std::ios::in);
    if(!input_file){
        std::cout<<"Couldn't open file "<<argv[1]<<std::endl;
        return -1;
    }

    output_file.open("out.bin", std::ios::out | std::ios::binary);

    int16_t tmp;
    while (input_file >> tmp) buffer.push_back(tmp);
    
    for (int i = 0; i < buffer.size(); i++){
        output_file.write(reinterpret_cast<const char *>(&buffer[i]), sizeof(buffer[i]));
    }

    input_file.close();
    output_file.close();

    return 0;
}