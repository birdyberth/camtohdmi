// --------------------------------------------------------------------
// Copyright (c) 2007 by Terasic Technologies Inc. 
// --------------------------------------------------------------------
//
// Permission:
//
//   Terasic grants permission to use and modify this code for use
//   in synthesis for all Terasic Development Boards and Altera Development 
//   Kits made by Terasic.  Other use of this code, including the selling 
//   ,duplication, or modification of any portion is strictly prohibited.
//
// Disclaimer:
//
//   This VHDL/Verilog or C/C++ source code is intended as a design reference
//   which illustrates how these types of functions can be implemented.
//   It is the user's responsibility to verify their design for
//   consistency and functionality through the use of formal
//   verification methods.  Terasic provides no warranty regarding the use 
//   or functionality of this code.
//
// --------------------------------------------------------------------
//           
//                     Terasic Technologies Inc
//                     356 Fu-Shin E. Rd Sec. 1. JhuBei City,
//                     HsinChu County, Taiwan
//                     302
//
//                     web: http://www.terasic.com/
//                     email: support@terasic.com
//
// --------------------------------------------------------------------

//`include "vpg.h"

module vpg(
	clk_50,
	reset_n,
	pixelrgb,
	vpg_pclk,
	vpg_de,
	vpg_hs,
	vpg_vs,
	vpg_r,
	vpg_g,
	vpg_b
);

input					clk_50;
input					reset_n;
input		[23:0]	pixelrgb;
output				vpg_pclk;
output				vpg_de;
output				vpg_hs;
output				vpg_vs;
output	[7:0]		vpg_r;
output	[7:0]		vpg_g;
output	[7:0]		vpg_b;

//=======================================================
//  Signal declarations
//=======================================================
wire vpg_pclk;
wire vpg_pclk_locked;

//============= assign timing constant  
wire  [11:0] h_total, h_sync, h_start, h_end; 
wire  [11:0] v_total, v_sync, v_start, v_end; 
wire  [11:0] v_active_14, v_active_24, v_active_34; 


//=======================================================
//  Sub-module
//=======================================================
//=============== PLL

pll hdmi_pll (
		.refclk   (clk_50),   
		.rst      (!reset_n),     
		.outclk_0 (vpg_pclk), 
		.locked   (vpg_pclk_locked)
	);


//=============== pattern generator according to vga timing
vga_generator u_vga_generator (                                    
	.clk(vpg_pclk),                
	.reset_n(vpg_pclk_locked),
	.pixelrgb(pixelrgb),
	.h_total(h_total),           
	.h_sync(h_sync),           
	.h_start(h_start),             
	.h_end(h_end),                                                    
	.v_total(v_total),           
	.v_sync(v_sync),            
	.v_start(v_start),           
	.v_end(v_end), 
	.v_active_14(v_active_14), 
	.v_active_24(v_active_24), 
	.v_active_34(v_active_34), 
	.vga_hs(vpg_hs),
	.vga_vs(vpg_vs),           
	.vga_de(vpg_de),
	.vga_r(vpg_r),
	.vga_g(vpg_g),
	.vga_b(vpg_b) );


//=======================================================
//  Structural coding
//=======================================================
//============= assign timing constant  
//h_total : total - 1
//h_sync : sync - 1
//h_start : sync + back porch - 1 - 2(delay)
//h_end : h_start + active
//v_total : total - 1
//v_sync : sync - 1
//v_start : sync + back porch - 1
//v_end : v_start + active
//v_active_14 : v_start + 1/4 active
//v_active_24 : v_start + 2/4 active
//v_active_34 : v_start + 3/4 active
//always @(mode)
//begin
//	case (mode)
//		0: begin //640x480@60 25.175 MHZ
//			{h_total, h_sync, h_start, h_end} <= {12'd799, 12'd95, 12'd141, 12'd781}; 
//			{v_total, v_sync, v_start, v_end} <= {12'd524, 12'd1, 12'd34, 12'd514}; 
//			{v_active_14, v_active_24, v_active_34} <= {12'd154, 12'd274, 12'd394};
//		end	
//		1: begin //720x480@60 27MHZ (VIC=3, 480P)
//			{h_total, h_sync, h_start, h_end} <= {12'd857, 12'd61, 12'd119, 12'd839}; 
//			{v_total, v_sync, v_start, v_end} <= {12'd524, 12'd5, 12'd35, 12'd515}; 
//			{v_active_14, v_active_24, v_active_34} <= {12'd155, 12'd275, 12'd395};
//		end
//		2: begin //1024x768@60 65MHZ (XGA)
//			{h_total, h_sync, h_start, h_end} <= {12'd1343, 12'd135, 12'd293, 12'd1317}; 
//			{v_total, v_sync, v_start, v_end} <= {12'd805, 12'd5, 12'd34, 12'd802}; 
//			{v_active_14, v_active_24, v_active_34} <= {12'd226, 12'd418, 12'd610};
//		end
//		3: begin //1280x1024@60   108MHZ (SXGA)
//			{h_total, h_sync, h_start, h_end} <= {12'd1687, 12'd111, 12'd357, 12'd1637}; 
//			{v_total, v_sync, v_start, v_end} <= {12'd1065, 12'd2, 12'd40, 12'd1064}; 
//			{v_active_14, v_active_24, v_active_34} <= {12'd296, 12'd552, 12'd808};
//		end	
//		4: begin //1920x1080p60 148.5MHZ (1080i)
//			{h_total, h_sync, h_start, h_end} <= {12'd2199, 12'd43, 12'd189, 12'd2109}; 
//			{v_total, v_sync, v_start, v_end} <= {12'd1124, 12'd4, 12'd40, 12'd1120}; 
//			{v_active_14, v_active_24, v_active_34} <= {12'd310, 12'd580, 12'd850};
//		end		
//		default: begin //1920x1080p60 148.5MHZ (1080i)
////			{h_total, h_sync, h_start, h_end} <= {12'd2199, 12'd43, 12'd189, 12'd2109}; 
////			{v_total, v_sync, v_start, v_end} <= {12'd1124, 12'd4, 12'd40, 12'd1120}; 
////			{v_active_14, v_active_24, v_active_34} <= {12'd310, 12'd580, 12'd850};
//			{h_total, h_sync, h_start, h_end} <= {12'd857, 12'd61, 12'd119, 12'd839}; 
//			{v_total, v_sync, v_start, v_end} <= {12'd524, 12'd5, 12'd35, 12'd515}; 
//			{v_active_14, v_active_24, v_active_34} <= {12'd155, 12'd275, 12'd395};
//		end
//	endcase
//end
assign h_total = 12'd1230;
assign h_sync = 12'd43;
assign h_start = 12'd50;
assign h_end = 12'd1130;

assign v_total = 12'd1933;
assign v_sync = 12'd3;
assign v_start = 12'd7;
assign v_end = 12'd1927;

assign v_active_14 = 12'd520;
assign v_active_24 = 12'd1000;
assign v_active_34 = 12'd1480;


endmodule