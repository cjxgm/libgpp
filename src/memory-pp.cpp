#include "libgpp.hpp"
#include <iostream>
#include <stdexcept>

namespace
{
    std::string output_buffer;
}

int main() try
{
    {
        std::cout << "Start preprocessing...\n";
        std::cout.flush();

        auto result = libgpp::preprocess(
            "#define BLOCK(X)    X\n"
            "#define EXPAND      BLOCK(#defeval DO_EXPAND #1#2#3#4#5#6#7#8#9\n"
            "DO_EXPAND)\n"
            "\n"
            "#define ANSWER_EVERYTHING    42\n"
            "#define ANSWER_TO(WHAT)      EXPAND(ANSWER_,WHAT)\n"
            "ANSWER_TO(EVERYTHING)        // Should give result: 42\n"
            "\n"
            "#define DOUBLE(X)            #eval (X)*2\n"
            "EXPAND(DOU,BLE,(21))         // Should give result: 42\n"
            "\n"
        );

        std::cout
            << "Preprocessed. Result:\n"
            << result
            << "\n"
        ;
        std::cout.flush();
    }

    {
        std::cout << "Start preprocessing...\n";
        std::cout.flush();

        output_buffer.clear();
        libgpp::preprocess_to_buffer(
            "#define BLOCK(X)    X\n"
            "#define EXPAND      BLOCK(#defeval DO_EXPAND #1#2#3#4#5#6#7#8#9\n"
            "DO_EXPAND)\n"
            "\n"
            "#define ANSWER_EVERYTHING    42\n"
            "#define ANSWER_TO(WHAT)      EXPAND(ANSWER_,WHAT)\n"
            "ANSWER_TO(EVERYTHING)        // Should give result: 42\n"
            "\n"
            "#define #DOUBLE(X)           #eval (X)*2\n"
            , output_buffer
        );

        std::cout
            << "SHOULD NOT REACH HERE:\n"
            << output_buffer
            << "\n"
        ;
        std::cout.flush();
    }
}
catch (std::exception const& e) {
    std::cerr << "\e[1;31m ERROR \e[0m " << e.what() << "\n";
    std::cerr << "Partial result:\n" << output_buffer << "\n";
}
catch (...) {
    std::cerr << "\e[1;31m UNKNOWN ERROR \e[0m\n";
    std::cerr << "Partial result:\n" << output_buffer << "\n";
}

