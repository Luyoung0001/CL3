#include <verilated.h>
#include <stdexcept>
#include <string>

void vl_fatal(const char* filename, int linenum, const char* hier, const char* msg) {
    Verilated::threadContextp()->gotError(true);
    Verilated::threadContextp()->gotFinish(true);

    std::string s = "Verilator $fatal: ";
    if (filename && filename[0]) {
        s += std::string(filename) + ":" + std::to_string(linenum) + ": ";
    }
    if (msg) s += msg;
    throw std::runtime_error(s);
}