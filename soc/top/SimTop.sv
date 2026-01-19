module top #(
    parameter logic [31:0] BOOT_ADDR = 'h80000000,
    parameter time CLK_HI = 5ns,
    parameter time CLK_LO = 5ns,
    parameter RESET_WAIT_CYCLES = 20,
    parameter TO_CNT = 100000000
);

// Add this empty function to make verilator add some header files (DPI).
export "DPI-C" function make_verilator_happy;

function void make_verilator_happy();
endfunction

`ifndef USE_VERILATOR
import "DPI-C" function void difftest_init(input string ref_so_file, input string img_file);

string ref_path, img_path;
initial begin
    void'($value$plusargs("ref=%s", ref_path));
    void'($value$plusargs("image=%s", img_path));
    if (ref_path != "" && img_path != "") begin
        $display("[TB] difftest_init ref=%s img=%s", ref_path, img_path);
        difftest_init(ref_path, img_path);
    end
end
`endif

logic       clk     = 'b1;
logic       rst_n   = 'b0;

initial begin: clock_gen 
    forever begin
        #(CLK_HI) clk = 1'b0;
        #(CLK_LO) clk = 1'b1;
    end
end

initial begin: reset_gen
    rst_n = 1'b0;
    repeat (RESET_WAIT_CYCLES) begin
        @(posedge clk);
    end
    rst_n = 1'b1;
    $display("[TESTBENCH] reset deasserted: %d", $time);
    $display("[TESTBENCH] note: disable waveform generation and difftest for better simulation performance");

end: reset_gen

// timeout
logic [31:0] cnt;
always_ff @( posedge clk or negedge rst_n ) begin : timeout
    if(~rst_n) begin
        cnt <= 'd0;
    end else begin
        cnt <= cnt + 'd1;
        if(cnt == TO_CNT) begin
            $display("[TESTBENCH] time out!");
            $finish;
        end
    end
end: timeout

initial begin: dump_wave
    longint unsigned dump_start, dump_stop;

    dump_start = 0;
    dump_stop  = '1;

    void'($value$plusargs("dump_start=%d", dump_start));
    void'($value$plusargs("dump_stop=%d",  dump_stop));

    wait (cnt >= dump_start);

    if($test$plusargs("vcd")) begin
        $dumpfile("wave/top.vcd");
        $dumpvars(0,top);
    end 
    if($test$plusargs("fst")) begin
        $display("[TESTBENCH] FST dump On at cycle %0d", cnt);
        $dumpfile("wave/top.fst");
        $dumpvars(0,top);
    end
`ifdef VCS
    if($test$plusargs("fsdb")) begin
        $fsdbDumpfile("wave/top.fsdb");
        $fsdbDumpvars(0,top);
    end
`endif
    wait (cnt >= dump_stop);

    if($test$plusargs("vcd") || $test$plusargs("fst")) begin
        $dumpoff();
        $display("[TB] VCD/FST dump OFF at cycle %0d", cnt);
    end

`ifdef VCS
    if($test$plusargs("fsdb")) begin
        $fsdbDumpoff();
        $display("[TB] FSDB dump OFF at cycle %0d", cnt);
    end
`endif

end: dump_wave

initial begin: load_hex
    string firmware;
    string dtbmem;
    int unsigned dtb_idx;
    longint unsigned dtb_addr;

    if($value$plusargs("firmware=%s",firmware)) begin
        $display("[TESTBENCH] loading firmware %s ...", firmware);
        #5; //We should add delay here to ensure the order of initial blocks.
        $readmemh(firmware, soc_top_u.axi_ram_u.mem);
        $display("[TESTBENCH] Check mem data ...");
        $display("[TESTBENCH] First data = %h", soc_top_u.axi_ram_u.mem[0]);
    end else begin
        $display("No firmware specified");
    end

    if ($value$plusargs("dtb_addr=%h", dtb_addr) & $value$plusargs("dtbmem=%s", dtbmem)) begin
        $display("[TESTBENCH] dtb path  = %s", dtbmem);
        $display("[TESTBENCH] dtb_addr  = 0x%08h", dtb_addr);
        dtb_idx = (dtb_addr - 32'h8000_0000) >> 2;
        $readmemh(dtbmem, soc_top_u.axi_ram_u.mem, dtb_idx);
    end else begin
        $display("No dtb specified");
    end
end


soc_top soc_top_u (
    .clk (clk),
    .rst_n (rst_n)
);

endmodule
