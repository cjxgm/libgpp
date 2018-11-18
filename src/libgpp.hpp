#pragma once
#include <string>

namespace libgpp
{
    auto preprocess_stdin_to_stdout() -> void;
    auto preprocess_to_buffer(std::string const& source, std::string& output) -> void;
    auto preprocess(std::string const& source) -> std::string;
}

