/*
Crashes on unload. Need to check sign when running with the amp. 11/30/18 -dtilley

*/
#include <iostream>
#include <math.h>
#include "If_PACI13.h"

using namespace std;
#define fastEXP RTmath->fastEXP //define shortcut for fastEXP function

// Required by RTXI
extern "C" Plugin::Object *createRTXIPlugin(void) {
    return new If_PACI13();
}

// These variables will automatically populate the Default GUI
// { "GUI LABEL", "GUI TOOLTIP", "VARIABLE TYPE",},
static DefaultGUIModel::variable_t vars[] = {    
    { "Voltage Input (V)", "Voltage Input (V)", DefaultGUIModel::INPUT, }, // input(0)
    { "Current Output (A)", "Current output (A)", DefaultGUIModel::OUTPUT, }, // output(0)
    { "BCL", "Basic cycle length for pacing (ms)", DefaultGUIModel::PARAMETER | DefaultGUIModel::INTEGER, },
    { "Stim Mag", "Stimulus magnitude (nA)", DefaultGUIModel::PARAMETER | DefaultGUIModel::DOUBLE, },
    { "Stim Length", "Stimulus length (ms)", DefaultGUIModel::PARAMETER | DefaultGUIModel::DOUBLE, },
    { "Pace", "(0: Pace off) (1: Pace on)", DefaultGUIModel::PARAMETER | DefaultGUIModel::INTEGER, },
    // Example state for outputing another variable
    { "Vm", "Membrane Potential", DefaultGUIModel::STATE ,},
    // Bonnie added
    { "Scale Factor", "Scaling target PACI13 If current", DefaultGUIModel::PARAMETER | DefaultGUIModel::DOUBLE, },
    { "If PACI13", "calculated If current from PACI13 model", DefaultGUIModel::STATE, },
    { "Cm", "Membrane capacitance (pF)", DefaultGUIModel::PARAMETER | DefaultGUIModel::DOUBLE, },
};

// Necessary RTXI variable *DO NOT EDIT*
static size_t num_vars = sizeof(vars) / sizeof(DefaultGUIModel::variable_t);

/*** Model Constructor ***/
If_PACI13::If_PACI13(void) :
    DefaultGUIModel("If_PACI13", ::vars, ::num_vars) {
    
    createGUI(vars, num_vars); // Function call to create graphical interface *can be overloaded for custom GUI*
 
    show(); // Show GUI
    initialize(); // Custom function for code organization, initializes parameters
    refresh(); // Refresh GUI
} // End constructor

/*** Model Destructor Function ***/
If_PACI13::~If_PACI13(void) {
    delete RTmath;
} // End destructor

/*** Realtime Execute Function ***/
void If_PACI13::execute() {
  out = 0; 
  V = input(0) * 1000 - 2.8; // in mV

  solve();
  out += (ipsc_i_f * cm); 
  output(0) = out; 

}

/*** Non-Realtime Update Function ***/
void If_PACI13::update(DefaultGUIModel::update_flags_t flag) {
    switch (flag) {

    case UNPAUSE: // Called when pause button is untoggled
        RTperiod = RT::System::getInstance()->getPeriod()*1e-6; // Grabs RTXI thread period and converts to ms (from ns)
        steps = static_cast<int>(ceil(RTperiod/DT)); // Determines number of iterations model solver will run based on RTXI thread rate

	scale = getParameter("Scale Factor").toDouble();
        break;
            
    case PAUSE: // Called when pause button is toggled
       
      out = 0; 
      scale = 0; 
        
        break;
            
    case MODIFY: // Called when modify button is hit                
      // Update Paramters
	scale = getParameter("Scale Factor").toDouble();
	bcl = getParameter("BCL").toInt();
	stimMag = getParameter("Stim Mag").toDouble();
	stimLength = getParameter("Stim Length").toDouble();
	pace = getParameter ("Pace").toInt();
	bclConverted = bcl/RTperiod; // Use unitless integers to avoid rounding errors
	stimLengthConverted = stimLength / RTperiod;
	cm = getParameter("Cm").toDouble();
        
        RTperiod = RT::System::getInstance()->getPeriod()*1e-6; // Grabs RTXI thread period and converts to ms (from ns)
        steps = static_cast<int>(ceil(RTperiod/DT)); // Determines number of iterations model solver will run based on RTXI thread rate

        break;
    default : 
      abort();
    }

}
    

/*** Parameter Initialization ***/
void If_PACI13::initialize() {
    RTmath = new RealTimeMath(); // RealTimeMath Library
    
    // Initial conditions and Constant initialization
    flag = 0;
    gf = 3.010312e-14; // A*(pF mV)^-1 maximal If conductance
    scale = 0; // 
    bcl= 1000;
    stimMag = 4;
    pace = 0;
    E_f=-17; // in mV
    count = 0; 
    cm = 20; // in pF
    
    
    // Set States and Parameters, connects GUI with actual variables
    setState("Vm", V);
    setState("If PACI13", ipsc_i_K1);
    setParameter("Scale Factor", scale);
    setParameter("BCL", bcl);
    setParameter("Stim Mag", stimMag);
    setParameter("Stim Length", stimLength);
    setParameter("Pace", pace);
    setParameter("Cm", cm);

    update(MODIFY); // Update user input parameters and rate dependent variables
}

/*** Current and gating solver ***/
void If_PACI13::solve() {
  
if ( pace == 1 ) { // Pacing
    if ( count < stimLengthConverted ) // Facing stimulus
      out += (stimMag * 1e-9); // Convert units (nA -> A)
    if ( count >= bclConverted ) // Reset count at end of beat
      count = 0;
    else 
      count++;
  }

    // If PACI13
 xf_inf = 1/(1.0+exp((V+77.85)/5));
 tauf = 1900/(1.0+exp((V+15)/10));
 // need to implement RealTime solver
 dXf_dt = (xf_inf-Xf)/tauf;

 // Doesn't work yet
 ipsc_i_f = gf * Xf * (V-E_f) * scale; // equation form Paci A/pF 


} //end solve()
