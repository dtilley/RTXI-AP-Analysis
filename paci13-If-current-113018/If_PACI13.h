

/*** Header Guard ***/
#ifndef If_PACI13_H
#define If_PACI13_H

#include <default_gui_model.h> // Standard RTXI Model
#include "include/RealTimeMath.h" // RealTimeMath library

/*** If_PACI13 Class ***/
class If_PACI13 : public DefaultGUIModel { // Inherits DefaultGUIModel

public:
    If_PACI13(void); // Constructor
    ~If_PACI13(void); // Destructor
    void update(update_flags_t flag); // Non-realtime update function
    void execute(void); // Realtime execute function

private:
    void solve(); // Run Euler Solver
    void initialize(); // Initialize parameters

    // Rate relevant parameters
    double RTperiod;
    int modelRate;
    double DT;
    int steps;

    // Optimized realtime math library with fastExp()
    RealTimeMath *RTmath;
    
    //// Parameters
    double V;
    int flag;
    
    //Ion Valences and Universal Constants	
    double R;
    double T;
    double F;
     
    //initial conditions taken from the LR code on the web
    double K_out;
    double E_K1;  
    double K_in;

    // Not updated -dtilley
    double PACI13_alpha;
    double PACI13_beta; 
    double PACI13_K1_inf;
    double ipsc_i_K1;
    double scale; 
    double gK1; 
    double count; 
    double bcl; 
    double pace;
    double stimMag;
    double stimLength;
    double bclConverted;
    double stimLengthConverted;
    double out; 
    double cm; 
};

#endif
