import difftest_pkg::*;
    module Difftest #(
      parameter int NR_COMMIT_PORTS = 2
    )(
      input logic clock,
      input logic reset,
      difftest_pkg::difftest_info_t [0 : NR_COMMIT_PORTS-1] diff_info
    );
    
    import "DPI-C" function int difftest_step(input int n, input difftest_pkg::difftest_info_t info[]);

    int ret;
    logic commit;

    difftest_pkg::difftest_info_t diff_info_q [0 : NR_COMMIT_PORTS-1] ;

    always_ff @(posedge clock) begin
      if (reset) begin
        foreach (diff_info_q[i]) begin
          diff_info_q[i] <= '{default: '0};
        end
      end else begin
        foreach (diff_info_q[i]) begin
          diff_info_q[i] <= diff_info[i];
        end
      end
    end

    always_comb begin

      commit = 1'b0;
      for(int i = 0; i < NR_COMMIT_PORTS; i++) begin
        commit = commit | diff_info_q[i].commit;
      end
    end

    always_ff @(posedge clock) begin

      if(commit) begin
        ret = 1'b0;
        ret = difftest_step(NR_COMMIT_PORTS, diff_info_q);
        if(ret) begin
          $fatal("HIT BAD TRAP!");
        end
      end
    end

  endmodule