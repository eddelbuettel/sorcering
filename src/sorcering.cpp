

// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <Rcpp/Benchmark/Timer.h>
using namespace Rcpp;

double w_mean(arma::colvec x, arma::colvec y)
    {
        double z;
        z =sum(x%y);
        return z;
    }
void pushfrontexception(const String& xtext ="",const String& listname ="")
    {
            String x1="";
            x1+=xtext;

            if (strlen(x1.get_cstring())>10)
            {   
                x1.push_front(listname); //x1 += listname;
                throw exception(x1.get_cstring()); 
            }
    }
NumericVector generate_input_vector(Nullable<List> in_list, int list_element, int list_length,const String& listname ="", int pools=0)
    {   
            String x1="";
            List in_listX(in_list);
            if (in_listX.size()!=list_length) x1 += " is not of size of C input file!"; 
            pushfrontexception(x1,listname);
            NumericVector in_element = in_listX[list_element];
            if (pools>0 && in_element.size()!=pools && listname!="C0_sl"  ) x1 += " contains elements that do not have the right size (number of pools) "; 
            if (pools>0 && in_element.size()!=pools && in_element.size()!=1 && listname=="C0_sl"  ) x1 += " contains elements that do not have the right size (number of pools) "; 
            pushfrontexception(x1,listname);
            return in_element;
    } 
    
NumericMatrix generate_input_matrix(Nullable<List> in_list, int list_element, int list_length,const String& listname ="", int pools=0, int timesteps=0)
    {   
            String x1="";
            List in_listX(in_list);
            if (list_length!=0 && in_listX.size()!=list_length) x1 += " is not of size of C input file!"; 
            if(!Rf_isMatrix(in_listX[list_element])) x1 += " is not a list containing matrices!";
            pushfrontexception(x1,listname);
            NumericMatrix  in_element = in_listX[list_element];
            if (pools>0 && in_element.ncol()!=pools && (listname!="A_sl" || (listname=="A_sl" && list_element!=0)) ) x1 += " contains elements that do not have the right number of columns (number of pools) "; //(listname!="A_sl" || (listname=="A_sl" && list_element!=0)) : exception when defining n_pools          
            pushfrontexception(x1,listname);
            if (timesteps>0 && in_element.nrow()!=timesteps && (listname!="A_sl" || (listname=="A_sl" && list_element!=0)) ) x1 += " contains elements that do not have the right number of rows (number of time steps). Make it match C input / first list element C input when modelling uncertainties! or Cinput*12 when env_in defining for modelling with annually timesteps"; //(listname!="A_sl" || (listname=="A_sl" && list_element!=0)) : exception when defining n_pools          
            pushfrontexception(x1,listname);
            return in_element;
    }    
List generate_input_list(Nullable<List> in_list, int list_element, int list_length,const String& listname ="",const String& misname ="")
    {   
            String x1="";
            List in_listX(in_list);
            if (list_length!=0 && in_listX.size()!=list_length) 
            {   
                x1 += " is not of correct size! "; 
                x1 += misname;
                x1 += " mismatch! "; 
            }
            pushfrontexception(x1,listname);            
            List in_element = in_listX[list_element];
            pushfrontexception(x1,listname);
            return in_element;
    }
void check_list(List inlist, const String& element ="", bool wood=0, const String& stand ="", const int layer =-1 )
    {
        for (int ilc = 0; ilc<inlist.size(); ilc++)
        {    
            String x1="";
            if(!Rf_isNumeric(inlist[ilc])) 
            {
                x1 +=element;
                x1 +=(" contains data not of type double! ");
            }
            if(!Rf_isMatrix(inlist[ilc])) 
            {
                x1 +=element;
                x1 +=(" must contain matrices! ");
            }
            if (strlen(x1.get_cstring())>10)
            {   
                if (strlen(stand.get_cstring())>0) 
                {
                    x1 +=" , Stand: ";
                    x1 += stand;
                }
                if (layer>=0) // only with unc (not non-stand-non-unc regular wood lists)
                {
                    if (wood==false) //no wood: layer is uncertainty level
                    {
                        x1 += ", uncertainty layer: ";
                        x1 += layer+1; 
                    }
                    else //wood: layer is wood level
                    {
                        x1 += ", wood layer: ";
                        x1 += layer+1; 
                    }                            
                }
                if (wood==true)
                {
                    if (layer>=0) // only with unc (not non-stand-non-unc regular wood lists) 
                    {
                        x1 += ", uncertainty layer: ";
                        x1 += std::to_string(ilc+1); 
                    }
                    else
                    {
                        x1 += ", wood layer: ";
                        x1 += std::to_string(ilc+1); 
                    }
                }
            }
            if (strlen(x1.get_cstring())>10) throw exception(x1.get_cstring());  
        }  
    }
List change_list_order(Nullable<List> in_list, const String& listname ="", const String& listname2 ="",int listnr=0) 
    {  
            String clow ="";
            List in_listX(in_list);
            int outer_size=in_listX.size();
            List InnerList = in_listX[0];
            int inner_size=InnerList.size();
            List NewList(inner_size);     
            for (int outer_i=0; outer_i<outer_size;outer_i++)
            { 
                if (strlen(listname2.get_cstring())>0) //multisite and wood and unc
                {
                    List wood_sl_;
                    wood_sl_ = generate_input_list(in_listX,outer_i,outer_size,listname,"site"); 
                    check_list(wood_sl_, listname, 1, listname2, outer_i );
                }
                else //wood and unc
                {
                    List wood_;
                    wood_ = generate_input_list(in_listX,outer_i,outer_size,listname,"uncertainties"); 
                    check_list(wood_, listname, 1, listname2, outer_i );
                }                                 
                InnerList = in_listX[outer_i];
                if (InnerList.size()!=inner_size) 
                {
                    clow +=(" Inconstistency in length of uncertainty lists of ");
                    clow +=listname; 
                    if (strlen(listname2.get_cstring())>0) 
                    {
                        clow +=(". Site: ");
                        String ls = std::to_string(listnr); 
                        clow +=ls;
                    }
                    clow +=(". 1st wood diameter -> length: ");
                    String isz = std::to_string(inner_size); 
                    clow += isz;
                    clow +=(". wood diameter ");
                    String oi = std::to_string(outer_i+1); 
                    clow += oi;
                    clow +=(" -> length: ");
                    String ils = std::to_string(InnerList.size()); 
                    clow += ils;
                    pushfrontexception(clow,listname);
                }
                for (int inner_i=0; inner_i<inner_size;inner_i++)  
                {
                    NumericMatrix  in_element =InnerList[inner_i];
                    List temp = NewList[inner_i];
                    temp.push_back(in_element);
                    NewList[inner_i]=temp;
                }
            }      
            return(NewList);
    }
bool contains_list(Nullable<List> in_list, int list_element)
    {
            List in_listX(in_list);
            SEXP x = in_listX[list_element];
            return TYPEOF(x) == VECSXP; //VECSXP=list
    }
NumericVector fT_yasso(arma::colvec T, arma::colvec P,double b1=7.6e-2, double b2=-8.9e-4,double g=-1.27)
    {
        NumericVector fT;
        arma::vec onevec;
        onevec.ones(P.n_elem);
        fT=exp(b1*T+b2*T%T)%(1-exp(g*P)); //exp(b1*T+b2*T%T)*(1-exp(g*P));
        return fT;
    }
List fastLm(const arma::vec & y, const arma::mat & X) 
    {
        int n = X.n_rows, k = X.n_cols;
        arma::colvec coef = arma::solve(X, y); 
        arma::colvec resid = y - X*coef; 
        double sig2 = arma::as_scalar(arma::trans(resid)*resid/(n-k));
        arma::colvec stderrest = 
        arma::sqrt(sig2 * arma::diagvec( arma::inv(arma::trans(X)*X)) );
    
        return List::create(Named("coefficients") = coef,
                            Named("stderr")       = stderrest,
                            Named("resid")       = resid);
    }
void checkwooddiamlist(int size, int wood_size, int uncert, int el_lists, bool unc=0, bool site=0, int c1n2=1)
{                                                            
        String sw ="";
        String w1 = std::to_string(size); 
        String w2 = std::to_string(wood_size);
        String w3 = std::to_string(uncert);
        String w4 = std::to_string(el_lists);
        sw +=(" Length of wood diameter input file: ");
        sw += w1;
        if (c1n2==1) sw +=("  does not match number of different wood classes of C input: ");
        if (c1n2==2) sw +=("  does not match number of different wood classes of N input: ");
        sw += w2;   
        if (unc) 
        {
            sw +=(". Uncert layer: ");
            sw += w3;
        }
        if (site) 
        {
            sw +=(". Site: ");
            sw += w4;  
        }
        throw exception(sw.get_cstring());
}  
void checkwooddims(int wood_size, List List_in, bool multisite, bool uncertain, int site, int unc, int tlen, int npools,int c1n2=1)
{
            arma::vec woodrows(wood_size);
            arma::vec woodcols(wood_size);   
            for (int wlc = 0; wlc<wood_size; wlc++)
            {
                NumericMatrix List_in_wood_mat = List_in[wlc];    
                woodcols(wlc) = List_in_wood_mat.ncol();
                woodrows(wlc) = List_in_wood_mat.nrow();

            }   
            int wr_c_num=0;
            int wr_r_num=0;
            String ws1 =""; 
            String wc3=std::to_string(site);
            String wc4=std::to_string(unc);
            String wc5="";
            bool colstrue=true;
            bool rowstrue=true;
            if (!all(woodcols==npools)) colstrue=false;
            if (!all(woodrows==tlen)) rowstrue=false;

            if (!colstrue) 
            {
                int wcl=0;
                bool woodwhile=true;
                while (woodwhile) 
                {
                    if (!colstrue && woodcols[wcl]!=npools) wr_c_num=woodcols[wcl];
                    if (wcl==wood_size-1 || (wr_c_num>0)) 
                    {
                        wc5=std::to_string(int(wcl+1));
                        woodwhile=false;
                    }
                    wcl+=1;
                }
                if (!colstrue && c1n2==1) ws1 +=(" Number of C pools (columns) of each wood input ");
                if (!colstrue && c1n2==2) ws1 +=(" Number of N pools (columns) of each wood input ");
                ws1 +=(" must match number of pools! At the moment: ");
                ws1 +=std::to_string(wr_c_num);
                ws1 +=(" But must be: ");
                ws1 +=std::to_string(npools);
            }
            if (!rowstrue) 
            {
                int wcl=0;
                bool woodwhile=true;
                while (woodwhile) 
                {
                    if (!rowstrue && woodrows[wcl]!=tlen) wr_r_num=woodrows[wcl];
                    if (wcl==wood_size-1 || (wr_r_num>0)) 
                    {
                        wc5=std::to_string(int(wcl+1));
                        woodwhile=false;
                    }
                    wcl+=1;
                }
                if (!rowstrue && c1n2==1) ws1 +=(" Number of time steps (rows) of each C wood input");
                if (!rowstrue && c1n2==2) ws1 +=(" Number of time steps (rows) of each N wood input");
                ws1 +=(" must match number of rows of C input! At the moment: ");
                ws1 +=std::to_string(wr_r_num);
                ws1 +=(" But must be: ");
                ws1 +=std::to_string(tlen);
            }
            if (strlen(ws1.get_cstring())>10) 
            {
                if (multisite)
                {
                    ws1 +=(" List element: ");
                    ws1 += wc3;   
                }    
                if (uncertain)
                {
                    ws1 +=(" Uncertainty layer: ");
                    ws1 += wc4;   
                }   
                ws1 +=(" Wood layer: ");
                ws1 += wc5;                   
                    throw exception(ws1.get_cstring());  
            }  
}
double fiom(double c)
    {
    double n;
    n=pow(10,(-1.31 + 1.139 * log10(c))); 
    return n;
    }
void set_seed(double seed) {
    Environment base_env("package:base");
    Function set_seed_r = base_env["set.seed"];
    set_seed_r(std::floor(std::fabs(seed)));
}
NumericVector C0_analyt(double CN_sp=0, double rmf=0, double t_a0=0, double t_a1=0, double t_a2=0, double t_a3=0, double s_a1=0, double s_a2=0, double Cmass=0, arma::rowvec fractI = {0.59, 0.41, 0})
    {
                NumericVector Cfracts;
                double fract_rooted_bio = 0.46;
                double fract_rooted_hum = 0.54;                
                
                double cue = 1/(1+ 1.67 * (1.85 + 1.6 * exp(-0.0786 * s_a1))); // the carbon use efficiency
                double alpha_1 = cue*fract_rooted_bio; // All the coefficients alpha.1 und alpha.2
                double alpha_2 = cue*fract_rooted_hum; // All the coefficients alpha.1 und alpha.2
                
                double a_1_1 = t_a2 * rmf * (alpha_1-1); // All the coefficients a.1.1, a.1.2, a.2.1, a2.2
                double a_1_2 = alpha_1 * t_a3 * rmf;
                double a_2_1 = alpha_2 * t_a2 * rmf;
                double a_2_2 = t_a3 * rmf * (alpha_2-1);
                
                double c_0_1 = (alpha_2*a_1_2 - alpha_1*a_2_2) / (a_1_1*a_2_2 - a_1_2*a_2_1); // The c.0.1; c.0.2; c.0.3 values
                double c_0_2 = (alpha_2*a_1_2 - alpha_1*a_2_2) / (a_1_1*a_2_2 - a_1_2*a_2_1);
                double c_0_3 = (a_1_2) / (a_1_1*a_2_2 - a_1_2*a_2_1);
                
                double u_bio_dpm=(c_0_2); // BIO pool quantification
                double u_bio_rpm=(c_0_1); 
                double u_bio_hum=(c_0_3); 
                
                double u_hum_dpm= 1/a_1_2*((-c_0_2*a_1_1-alpha_1)); // HUM pool quantification ( is all C.78)
                double u_hum_rpm= 1/a_1_2*(-c_0_2*a_1_1-alpha_1);
                double u_hum_hum= 1/a_1_2*(-c_0_3*a_1_1);
                
                double u_dpm_dpm=1/t_a0/rmf; // DPM C ( is all C.79)
                double u_rpm_rpm=1/t_a1/rmf; //RPM C ( is all C_80)  //bug: exchanged t_a0 by t_a1 March 18, 2025
                
                double u_dpm=u_dpm_dpm+u_bio_dpm+u_hum_dpm; // Total C ( is all C_78)
                double u_rpm=u_rpm_rpm+u_bio_rpm+u_hum_rpm;
                double u_hum=u_bio_hum+u_hum_hum;
                
                double Nenner= fractI(0)*u_dpm+fractI(1)*u_rpm+fractI(2)*u_hum;
                
                double fract_dpm= fractI(0)*u_dpm_dpm/Nenner;
                double fract_rpm= fractI(1)*u_rpm_rpm/Nenner;
                double fract_bio= (fractI(0)*u_bio_dpm+fractI(1)*u_bio_rpm+fractI(2)*u_bio_hum)/Nenner;
                double fract_hum= (fractI(0)*u_hum_dpm+fractI(1)*u_hum_rpm+fractI(2)*u_hum_hum)/Nenner;   

                arma::rowvec fract_all =   {fract_dpm,fract_rpm,fract_bio,fract_hum} ;
                
                double IOM;
                
                if ((s_a2==1) && (CN_sp>11))  //Springob Method
                { 
                    double c_old = Cmass;
                    double reli_soc = c_old/CN_sp*(11-CN_sp)/(11/35-1);
                    double c_new = c_old - reli_soc;
                    IOM = fiom(c_new) + reli_soc ;
                }
                else IOM = fiom(Cmass);  

                fract_all = fract_all * (Cmass-IOM) / Cmass;
                Cfracts={fract_all(0)*Cmass,fract_all(1)*Cmass,fract_all(2)*Cmass,fract_all(3)*Cmass,IOM};       
                
                return Cfracts;
    }

// [[Rcpp::export]]

List
sorcering(
              const Nullable<NumericMatrix> A = R_NilValue, 
              const String& tsteps ="monthly", 
              const Nullable<NumericVector> C0 = R_NilValue, 
              const Nullable<NumericVector> N0 = R_NilValue, 
              const Nullable<SEXP*> Cin = R_NilValue,
              const Nullable<SEXP*> Nin = R_NilValue,
              const Nullable<List> Cin_wood = R_NilValue,
              const Nullable<List> Nin_wood = R_NilValue,
              const Nullable<NumericVector> wood_diam = R_NilValue,
              const Nullable<SEXP*> xi = R_NilValue,
              const Nullable<NumericMatrix> env_in = R_NilValue,
              const Nullable<NumericVector> site = R_NilValue,
              const Nullable<SEXP*> theta = R_NilValue,
              const Nullable<NumericVector> theta_unc = R_NilValue,
              const int theta_n_unc = 1,
              const Nullable<NumericMatrix> meas_data = R_NilValue, 
              const Nullable<List> A_sl = R_NilValue, 
              const Nullable<List> C0_sl = R_NilValue,
              const Nullable<List> N0_sl = R_NilValue,
              const Nullable<List> Cin_sl = R_NilValue,
              const Nullable<List> Nin_sl = R_NilValue,
              const Nullable<List> Cin_wood_sl = R_NilValue,
              const Nullable<List> Nin_wood_sl = R_NilValue,
              const Nullable<List> wood_diam_sl = R_NilValue,
              const Nullable<List> xi_sl = R_NilValue,
              const Nullable<List> env_in_sl = R_NilValue,
              const Nullable<List> site_sl = R_NilValue,
              const Nullable<List> sitelist = R_NilValue,
              const Nullable<List> meas_data_sl = R_NilValue,
              const bool calcN = false,
              const bool calcNbalance = false,
              const bool calcN0 = false,
              const bool calcC0 = false,
              const bool calcCN_fast_init = false,
              const bool CTool_input_raw = false,
              const bool RothC_Cin4C0 = false,
              const NumericVector& RothC_dpmrpm = 1.439024, 
              const Nullable<NumericVector> C0_fracts = R_NilValue, 
              const bool multisite = false,
              const Nullable<IntegerVector> pooltypes = R_NilValue, 
              const int CN_fast_init = 40,
              const int CN_bio = 9,
              const Nullable<NumericVector> CN_spin = R_NilValue,
              const Nullable<List> CN_fast_init_sl = R_NilValue,
              const Nullable<List> CN_bio_sl = R_NilValue,
              const Nullable<List> CN_spin_sl = R_NilValue,
              const bool init_info = false,
              const String& model ="",
              const bool spinup = false,
              const int t_spin = 2,
              const List t_spin_sl = 2
             ) 
{    

    // sorcering v1.2, June 2, 2025
    // SORCERING: Soil ORganic Carbon & CN Ratio drIven Nitrogen modellinG framework
    // by 
    // Dr. Marc Scherstjanoi, Thünen Institute of Forest Ecosystems, Eberswalde, Germany &
    // Dr. René Dechow, Thünen Institute of Climate-Smart Agriculture, Braunschweig, Germany
    
    Environment base = Environment("package:base");
    Function readline = base["readline"];
     
    std::string yasso20 = "Yasso20";  
    std::string yasso15 = "Yasso15";  
    std::string yasso07 = "Yasso07";
    std::string ctool = "C-Tool";
    std::string ctool_o = "C-Tool-org";
    std::string rothc = "RothC";
    
    bool someyasso = FALSE;
    bool yasso15up = FALSE;
    bool cin_uncertain = FALSE;
    bool nin_uncertain = FALSE;  
    bool xi_uncertain = FALSE;  
    bool any_unc = FALSE;
    if (model.get_cstring()==yasso20 || model.get_cstring()==yasso15 || model.get_cstring()==yasso07) someyasso = TRUE;
    if (model.get_cstring()==yasso20 || model.get_cstring()==yasso15) yasso15up = TRUE;
    
    //////////////////////////////////////////////////////////////////////////////////////////////////////////
    //data type check
    //////////////////////////////////////////////////////////////////////////////////////////////////////////

    String c1 =""; 
    String c1b =""; 
    String c2 =""; 
    if (!multisite)
    {
        if (!spinup && !calcC0 && C0.isNull()) c1 +=("C0 has not been defined.");
        if (!spinup && calcN && !calcN0 && N0.isNull() ) c1 +=("N0 has not been defined. ");
        //if (spinup && !calcC0 && !C0.isNull()) c1 +=("Starting spinup with a pre-defined C0.");
        //if (spinup && calcN && !calcN0 && !N0.isNull() ) c1 +=("Starting spinup with a pre-defined N0.");
        //if (spinup && calcC0) c1 +=("Starting spinup with a calculated C0.");
        //if (spinup && calcN0) c1 +=("Starting spinup with a calculated N0.");
        if (Nin.isNull() && Nin_wood.isNull() && calcN) c1 +=("Nin has not been defined. ");
        if (xi.isNull() && (strlen(model.get_cstring())==0)) c1 +=("xi has not been defined. ");
        if (strlen(c1.get_cstring())>10) c1 +=("Was that on purpose? ");
        if (!yasso15up && !Cin_wood.isNull()) c1 +=("Cin_wood has been defined although not modelling with Yasso15 or Yasso20.");
        if (yasso15up && wood_diam.isNull() && !Cin_wood.isNull()) c1 +=("wood_diam has not been defined! Cin_wood has no effect this way! ");
        if (strlen(c1.get_cstring())>10) Rcout << c1.get_cstring() << " \n";
        if (!Rf_isNumeric(Cin) && !Cin.isNull() && TYPEOF(Cin)!=VECSXP ) c2 +=("Cin is not of type double (maybe try 'Cin=as.matrix(Cin)') ");
        if (!Rf_isNumeric(Nin) && !Nin.isNull()  && TYPEOF(Nin)!=VECSXP && calcN) c2 +=("Nin is not of type double (maybe try 'Nin=as.matrix(Nin)') ");
        if (!Cin.isNull() && TYPEOF(Cin)!=VECSXP && !Rf_isMatrix(Cin)) c2 +=("Cin is neither matrix nor list. ");
        if (!Nin.isNull() && TYPEOF(Nin)!=VECSXP && !Rf_isMatrix(Nin) && calcN) c2 +=("Nin is neither matrix nor list. ");     
        if (!xi.isNull() && TYPEOF(xi)!=VECSXP && !Rf_isMatrix(xi)) c2 +=("xi is neither matrix nor list. ");
        if (calcC0 || calcN0)
        {   
            if (meas_data.isNull() ) c2 +=("meas_data has not been defined");
            else if (!Rf_isNumeric(meas_data)) c2 +=("meas_data is not of type double (maybe try 'meas_data=as.matrix(meas_data)') ");
        }
        if (strlen(c2.get_cstring())>10) throw exception(c2.get_cstring()); 
    }

    if (multisite)
    {                 
        List const t_spinlist(t_spin_sl);  
        if (spinup)
        {
            int listlength = t_spinlist.size();
            for(int el_ts=0; el_ts<listlength; el_ts++)  
            {
                int tspinlist=t_spinlist[el_ts];
                if (tspinlist<=2) 
                {
                    c1b +=("At least one stand has fever than 3 simulations time steps for the spinup only. Was that on purpose? Did you define t_spin_sl?");
                    break;
                }
            }
        }
        if (strlen(c1b.get_cstring())>10) Rcout << c1b.get_cstring() << " \n";
        if (!spinup && calcC0==false && C0_sl.isNull() && model.get_cstring()!=ctool_o) c1 +=("C0_sl has not been defined. ");
        if (!spinup && calcN && calcN0==false && N0_sl.isNull() && model.get_cstring()!=ctool_o) c1 +=("N0_sl has not been defined. ");
        //if (spinup && !calcC0 && !C0_sl.isNull() && model.get_cstring()!=ctool_o) c1 +=("Starting spinup with a pre-defined C0_sl.");
        //if (spinup && calcN && !calcN0 && !N0_sl.isNull() && model.get_cstring()!=ctool_o) c1 +=("Starting spinup with a pre-defined N0_sl.");       
        //if (spinup && calcC0) c1 +=("Starting spinup with a calculated C0.");
        //if (spinup && calcN0) c1 +=("Starting spinup with a calculated N0.");
        if (Cin_sl.isNull() && Cin_wood_sl.isNull()) c1 +=("Cin_sl has not been defined. ");
        if (Nin_sl.isNull() && Nin_wood_sl.isNull() && calcN) c1 +=("Nin_sl has not been defined. ");
        if (strlen(c1.get_cstring())>10) c1 +=("Was that on purpose? ");
        if (!yasso15up && !Cin_wood_sl.isNull()) c1 +=("Cin_wood_sl has been defined although not modelling with Yasso15 or Yasso20.");
        if (yasso15up && wood_diam_sl.isNull() && wood_diam.isNull() && !Cin_wood_sl.isNull() ) c1 +=("wood_diam_sl has not been defined! Cin_wood_sl has no effect this way!");
        if (strlen(c1.get_cstring())>10) Rcout << c1.get_cstring() << " \n";
        if (calcN && (!Nin_sl.isNull() || !Cin_sl.isNull()) )
        {
          List Cin_slX(Cin_sl);
          List Nin_slX(Nin_sl);
          if (Cin_slX.size() != Nin_slX.size()) c2 +=("Cin_sl and Nin_sl must have the same number of elements");         
        }     
        if (calcN && (!Nin_wood_sl.isNull() || !Cin_wood_sl.isNull()) )
        {
          List Cin_wood_slX(Cin_wood_sl);
          List Nin_wood_slX(Nin_wood_sl);
          if (Cin_wood_slX.size() != Nin_wood_slX.size()) c2 +=("Cin_wood_sl and Nin_wood_sl must have the same number of elements");         
        }  
        if (sitelist.isNull()) c2 +=("sitelist has not been defined.");        
        if (xi_sl.isNull() && (strlen(model.get_cstring())==0)) c2 +=("xi_sl has not been defined. ");
        if ((calcC0 || calcN0) && (meas_data_sl.isNull() ) ) c2 +=("meas_data_sl has not been defined"); 
        if (strlen(c2.get_cstring())>10) throw exception(c2.get_cstring());  

    }

    //time stuff
    std::string annually = "annually";
    std::string monthly = "monthly";
    std::string weekly = "weekly";
     
    int timediv = 12;
    if (tsteps.get_cstring()==monthly)timediv=12;
    if (tsteps.get_cstring()==annually)timediv=1;
    if (tsteps.get_cstring()==weekly)timediv=52;

    int nr_pools=2;
    bool model_def=false;
    int wood_sz =1; //wood_size, only used for yasso
  
    //check model status
    String m1 ="";
    if (strlen(model.get_cstring())>0)  
    {
        if (!someyasso && model.get_cstring()!=rothc && model.get_cstring()!=ctool && model.get_cstring()!=ctool_o) 
        {
            m1 +=(" 'model' must be one of the following: 'Yasso20', 'Yasso15', 'Yasso07', 'RothC', 'C-Tool', 'C-Tool-org'!");
            if (strlen(m1.get_cstring())>10)  throw exception(m1.get_cstring());
        }
        else 
        {
            model_def = true;
            String cm1 =""; 
            String cm2 =""; 
            
            if (!multisite && env_in.isNull()) cm1 +=("env_in has not been defined. ");
            if (!multisite && !someyasso && site.isNull()) cm1 +=("site has not been defined. ");//other models but yasso
            if (multisite)
            {
                if (env_in_sl.isNull()) cm1 +=("env_in_sl has not been defined. ");    
                if (!someyasso && site_sl.isNull()) cm1 +=("site_sl has not been defined. ");//other models but yasso
            }
            if (strlen(cm1.get_cstring())>10) throw exception(cm1.get_cstring());  
            if (!multisite)
            {
                if (!Rf_isNumeric(env_in)) cm2 +=("env_in is not of type double (maybe try 'env_in=as.matrix(env_in)') ");
                if (!someyasso && !Rf_isNumeric(site)) cm2 +=("site is not of type double"); //other models but yasso
            }
            if (strlen(cm2.get_cstring())>10) throw exception(cm2.get_cstring());  
        }
    }

    //////////////////////////////////////////////////////////////////////////////////////////////////////////
    //input transformation
    //////////////////////////////////////////////////////////////////////////////////////////////////////////
    
    // transfer matrix stuff
    NumericMatrix A_(nr_pools,nr_pools);   //if no A 
    if (A.isNotNull()) 
    {   
        NumericMatrix AX(A);
        A_=AX;
    }
    NumericMatrix A_sl0_(nr_pools,nr_pools); 
    if (multisite && A_sl.isNotNull())
    {
        NumericMatrix A_sl_(nr_pools,nr_pools); 
        A_sl0_ = generate_input_matrix(A_sl, 0,0,"A_sl",nr_pools,0); //take from first list element
    }

    arma::mat A_arma = as<arma::mat>(A_);
    arma::mat A_arma_yasso_standard = A_arma; //define A_arma_yasso_standard here. No need to calculate A_arma for every stand. Only depends on parameters.
    
    // define nr_pools  
    if (model_def && (someyasso || model.get_cstring()==rothc)) nr_pools=5;
    if (model_def && (model.get_cstring()==ctool || model.get_cstring()==ctool_o )) nr_pools=6; 
    if (!model_def) 
    {
        if (!multisite || A_sl.isNull()) nr_pools = A_arma.n_rows;   
        if (multisite && A_sl.isNotNull()) nr_pools = A_sl0_.nrow();  
    }

    //wood stuff
    bool wood = false;
    if ((multisite && Cin_wood_sl.isNotNull()) || (!multisite && Cin_wood.isNotNull()) ) wood = true;
    if (!multisite && wood)
    {
        List list_Cin_wood(Cin_wood);
        List list_Nin_wood(Nin_wood);
        int Cin_wood_size=-1;
        int Nin_wood_size=-1;
          
        if (!contains_list(list_Cin_wood,0)) //contains list -> uncertain, do not check here
        {
            Cin_wood_size = list_Cin_wood.size();   
            wood_sz = Cin_wood_size;
            check_list(list_Cin_wood, "Cin_wood", 1, "", -1 );
        }
        if (calcN && !contains_list(list_Nin_wood,0))
        {
            Nin_wood_size = list_Nin_wood.size();
            check_list(list_Nin_wood, "Nin_wood", 1, "", -1 );
        }
        if (Cin_wood_size !=Nin_wood_size && Cin_wood_size>0 && Nin_wood_size>0) throw exception("Cin_wood and Nin_wood must have the same number of wood layers!"); 
    }
    //////////////////////////////////////////
    // new with v1.1.0, read t_sim from Cin //
    //////////////////////////////////////////
    int tsim_list_=2;
    List t_list;
    int list_length=1; 
    int Cin_rows=-1; 
    if (multisite)
    {  
        if (sitelist.isNotNull()) 
        {   
            List const sitelistXX(sitelist); 
            list_length = sitelistXX.size();
        }
        for(int el_lists=0; el_lists<list_length; el_lists++)  
        {
            if (wood)
            {
                if (Cin_wood_sl.isNotNull()) 
                { 
                    List Cin_wood_list_ = generate_input_list(Cin_wood_sl, el_lists,list_length,"Cin_wood_sl","site"); 
                    if (contains_list(Cin_wood_list_,0)) //wood and uncertain
                    {    
                        List Cin_wood_u_list_ = generate_input_list(Cin_wood_list_, 0,0,"Cin_wood_list_","site"); 
                        check_list(Cin_wood_u_list_, "Cin_wood_sl", 1, "", 0 );
                        NumericMatrix Cin_wood_sl_0 = generate_input_matrix(Cin_wood_u_list_,0,0,"At least one uncertainty layer of Cin_wood_sl",nr_pools,0); 
                        Cin_rows=Cin_wood_sl_0.nrow(); 
                    }
                    else
                    {
                        check_list(Cin_wood_list_, "Cin_wood_sl", 1, "", -1 );
                        NumericMatrix Cin_wood_sl_0 = generate_input_matrix(Cin_wood_list_, 0,0,"Cin_wood_sl",nr_pools,0); 
                        Cin_rows=Cin_wood_sl_0.nrow();                        
                    }
                }  
            }
            else
            {
                if (Cin_sl.isNotNull()) 
                {   
                    if (contains_list(Cin_sl,0)) 
                    {
                        List Cin_u_list_ = generate_input_list(Cin_sl, el_lists,list_length,"Cin_sl","site"); 
                        check_list(Cin_u_list_, "Cin_sl", 0, "", 0 );
                        NumericMatrix Cin_sl_0 = generate_input_matrix(Cin_u_list_,0,0,"At least one uncertainty layer of Cin_sl",nr_pools,0); 
                        Cin_rows=Cin_sl_0.nrow(); 
                    }
                    else 
                    {
                        NumericMatrix Cin_sl_0 = generate_input_matrix(Cin_sl, el_lists,list_length,"Cin_sl",nr_pools,0); 
                        Cin_rows=Cin_sl_0.nrow(); 
                    }
                }
                else
                {
                    throw exception("Either Cin, Cin_sl, Cin_wood or Cin_wood_sl must be defined! 'No input' simulation? Fill with zeros!"); 
                }
            }
            if (Cin_rows-floor(Cin_rows/timediv)*timediv!=0) 
            {
                if (timediv==12) Rcout << "Warning! tsteps=monthly and number of rows of carbon input file is not a multiple of 12! List element: " << el_lists <<" \n";
                if (timediv==52) Rcout << "Warning! tsteps=weekly and number of rows of carbon input file is not a multiple of 52! List element: " << el_lists <<" \n";
            }
            t_list.push_back(Cin_rows);
        }  
    }
    else
    {
        if (wood) 
        {
            if (Cin_wood.isNotNull()) 
            { 
                SEXP Cin_wood0 = generate_input_list(Cin_wood, 0,0,"Cin_wood_sl","site"); 
                if (contains_list(Cin_wood0,0)) //wood and uncertain
                {  
                    List Cin_wood_list_ = generate_input_list(Cin_wood0, 0,0,"Cin_wood_list_","site"); 
                    check_list(Cin_wood_list_, "Cin_wood", 1, "", 0 );
                    NumericMatrix Cin_wood_0 = generate_input_matrix(Cin_wood_list_,0,0,"At least one uncertainty layer of Cin_wood_sl",nr_pools,0); 
                    Cin_rows=Cin_wood_0.nrow(); 
                }
                else
                {
                    List list_Cin_wood(Cin_wood);
                    if (TYPEOF(Cin_wood0) == VECSXP) 
                    {
                        check_list(list_Cin_wood, "Cin_wood", 1, "", -1 );
                        NumericMatrix Cin_wood_0 = generate_input_matrix(list_Cin_wood, 0,0,"Cin_wood",nr_pools,0); 
                        Cin_rows=Cin_wood_0.nrow();   
                    }
                }                
            }  
        }
        else
        {
            if (Cin.isNotNull()) 
            {  
                if (TYPEOF(Cin) == VECSXP) 
                {
                    List Cin_0X(Cin);
                    check_list(Cin_0X, "Cin", 0, "", 0 );
                    NumericMatrix Cin_0 = generate_input_matrix(Cin_0X,0,0,"At least one uncertainty layer of Cin_sl",nr_pools,0); 
                    Cin_rows=Cin_0.nrow(); 
                }
                else
                {
                    NumericMatrix Cin_0(Cin);
                    Cin_rows=Cin_0.nrow(); 
                }
            }
            else
            {
                throw exception("Either Cin, Cin_sl, Cin_wood or Cin_wood_sl must be defined! 'No input' simulation? Fill with zeros!"); 
            }
        }   
        if (Cin_rows-floor(Cin_rows/timediv)*timediv!=0)
        {
            if (timediv==12) Rcout << "Warning! tsteps=monthly and number of rows of carbon input file is not a multiple of 12!" <<" \n";
            if (timediv==52) Rcout << "Warning! tsteps=weekly and number of rows of carbon input file is not a multiple of 52!" <<" \n";
        }
    }
    //////////////////////////////////////////
    // end read t_sim from Cin ///////////////
    //////////////////////////////////////////
    
    int tsim=Cin_rows;
    int tspin=t_spin;
 
    //model specific definitions
    int sitecol=0;
    int thetacol=1;
    if (model.get_cstring()==rothc) sitecol=4;
    if (model.get_cstring()==ctool || model.get_cstring()==ctool_o) sitecol=1;
    IntegerVector pooltypes_ (nr_pools);  // 1: fast, 2:bio/humus, 3:inert/res, 4:fast sub, 5:bio/humus sub, 6: inert/res sub
    if (yasso15up) {thetacol=30; pooltypes_={1,1,1,2,3}; }
    if (model.get_cstring()==yasso07) {thetacol=21; pooltypes_={1,1,1,2,3}; }
    if (model.get_cstring()==rothc) {thetacol=7; pooltypes_={1,1,2,2,3}; }
    if (model.get_cstring()==ctool || model.get_cstring()==ctool_o) {thetacol=10; pooltypes_={1,2,3,4,5,6}; }
            
    // input, starting conditions, environment
    NumericVector C0_ (nr_pools); 
    NumericVector C0_fracts_ (nr_pools); 
    NumericVector N0_ = C0_/10;
    NumericVector site_ (0);
    NumericVector theta_ (thetacol);
    NumericMatrix thetamat_ ;
    NumericVector thetafac_ (thetacol);
    NumericVector CN_spin_ (0);
    arma::vec RothC_dpmrpm_arma=RothC_dpmrpm;
    
    if (C0.isNotNull()) C0_=C0;
    if (C0_fracts.isNotNull())  C0_fracts_=C0_fracts;
    else 
    {
        if (someyasso) C0_fracts_= {0.15,0.025,0.025,0.35,0.45};
        if (model.get_cstring()==ctool) C0_fracts_= {0.0316,0.4804,0.488,0.00338298, 0.35216822, 0.77210880};
    }
    if (N0.isNotNull()) N0_=N0;
    if (site.isNotNull()) site_=site;
    if (CN_spin.isNotNull()) CN_spin_=CN_spin;

    //model parameters
    if (theta.isNotNull()) 
    { 
        if (Rf_isMatrix(theta))
        {  
            if (theta_unc.isNotNull()) Rcout << "theta_unc will have no effect when theta is a matrix. ";
            NumericMatrix thetaX(theta);
            thetamat_=thetaX;
        }    
        else if (Rf_isVector(theta)) 
        {
            theta_=theta;   
            if (model_def && theta_.size()!=thetacol) 
            {
                String cr1 = ("theta must be of length: ");
                cr1 +=  thetacol;
                throw exception(cr1.get_cstring());        
            }
        }
        else throw exception("theta neither vector nor matrix! ");  
    }

    if (!(Rf_isMatrix(theta)) && theta_unc.isNotNull())  
    { 
            NumericVector theta_unc_(theta_unc); 
            int thetalength = theta_unc_.size();
            if (thetalength!=1 && thetalength!=thetacol) throw exception("theta_unc not of length 1 or length of theta");
            if (thetalength==1) 
            {
                double thetafill=theta_unc_[0]/100;
                thetafac_.fill(thetafill);
            }
            if (thetalength==thetacol) thetafac_=theta_unc_/100;
    }

    if ((theta.isNull()) && model.get_cstring()==yasso20) theta_= {0.51, 5.19, 0.13, 0.1, 0.0015, 0.5, 0, 1, 1, 0.99, 0, 0, 0, 0, 0, 0.163, 0, 0.0015, 0.158, -0.002, 0.17, -0.005, 0.067, 0, -1.44, -2, -6.9, -2.55, 1.24, 0.25};
    if ((theta.isNull()) && model.get_cstring()==yasso15) theta_= {0.49, 4.9, 0.24, 0.095, 0.0013, 0.44, 0.25, 0.92, 0.99, 0.084, 0.011, 0.00061, 0.00048, 0.066, 0.00077, 0.1, 0.65, 0.0046, 0.091, -0.00021, 0.049, -0.000079, 0.035, -0.00021, -1.8, -1.2, -13, -0.44, 1.3, 0.26};
    if ((theta.isNull()) && model.get_cstring()==yasso07) theta_= {0.66, 4.3, 0.35, 0.22, 0.0033, 0.32, 0.01, 0.93, 0.34, 0, 0, 0, 0, 0.01, 0, 0, 0.92, 0.04, 7.6e-2, -8.9e-4, -1.27}; 
    if ((theta.isNull()) && model.get_cstring()==rothc) theta_= {10.0, 0.3, 0.66, 0.02, 0.0, 1.0, 0.2};
    if ((theta.isNull()) && model.get_cstring()==ctool) theta_= {1.44, 0.0336, 4.63e-4, 1.44, 0.0336, 4.63e-4, 0.03, 0.628, 0.012, 0};
    if ((theta.isNull()) && model.get_cstring()==ctool_o) theta_= {1.44, 0.0336, 0, 1.44, 0.0336, 0, 0, 0.628, 0, 0.358};
    if ((!model_def) && (pooltypes.isNotNull()) ) pooltypes_=pooltypes;

    NumericMatrix Cin_(tsim,nr_pools); 
    NumericMatrix Nin_ = Cin_/10;
    NumericVector v (tsim*nr_pools,1.0);
    NumericMatrix xi_(tsim,nr_pools,v.begin());
    NumericMatrix meas_data_(2,4);
    int env_in_length = 2;
    if (model.get_cstring()==rothc) env_in_length=4;
    NumericMatrix env_in_(tsim,env_in_length,v.begin());

    List Cin_wood_;
    List Nin_wood_;    
    List Cin_u_;
    List Nin_u_;  
    List xi_u_;

    arma::rowvec wood_xi(nr_pools,arma::fill::ones);
    NumericVector wood_diam_; 
    if (wood_diam.isNotNull()) wood_diam_=wood_diam;

    //variable transformation due to R_NilValue
    if (!multisite)
    {
        if (Cin.isNotNull()) 
        {
            if (TYPEOF(Cin) == VECSXP) 
            {
                List Cin_uX(Cin);
                Cin_u_=Cin_uX;
            }
            else
            {
                NumericMatrix CinX(Cin);
                Cin_=CinX;
            }
        }

        if (Nin.isNotNull()) 
        {
            if (TYPEOF(Nin) == VECSXP)
            {
                List Nin_uX(Nin);
                Nin_u_=Nin_uX;
            }
            else
            {
                NumericMatrix NinX(Nin);
                Nin_=NinX;
                if (Nin_.nrow()!=tsim) throw exception( "Nin has not the right number of rows (number of time steps). Make it match C input! ");   
            }
        }
        if (xi.isNotNull()) 
        {   
            if (TYPEOF(xi) == VECSXP)
            {
                List xi_uX(xi);
                xi_u_=xi_uX;
            }
            else
            {
                NumericMatrix xiX(xi);
                xi_=xiX;
            }
        }
        if (meas_data.isNotNull()) 
        {   
            NumericMatrix meas_dataX(meas_data);
            meas_data_=meas_dataX;
        }
        if (env_in.isNotNull()) 
        {   
            NumericMatrix env_inX(env_in);
            env_in_=env_inX;
            if ((timediv!=1) && (env_in_.nrow()!=tsim)) throw exception( "env_in has not the right number of rows (number of time steps). Make it match C input! ");
            if ((timediv==1) && (env_in_.nrow()!=tsim*12)) throw exception( "env_in has not the right number of rows (number of time steps). Make it match C input times 12 when using annual timesteps! ");
        }
        if (Cin_wood.isNotNull())
        {
            List Cin_woodX(Cin_wood);
            Cin_wood_ = Cin_woodX;
        }
        if (Nin_wood.isNotNull()) 
        {
            List Nin_woodX(Nin_wood);
            Nin_wood_ = Nin_woodX;
        }  
    }
    // prepare listlength global variables for generating output
    int listlength = 1; //c always out
    if (calcN) listlength+=(nr_pools+3);
    if (calcNbalance) listlength+=1;
    
    List listout (listlength);  //list of results for normal mode
    List listout2 (listlength); //list of single stand results for list mode 
    List listout_list; // total list output for multisite, includes all listout2s

    //int tsim_list_=2;
    int tspin_list_=2;   
    bool donotwrite=FALSE;
    // loop over list elements
    for(int el_lists=0; el_lists<list_length; el_lists++)  
    {
                        double CNfast=CN_fast_init;
                        double CNbio=CN_bio;

                        if (multisite) 
                        {
                            tsim_list_=t_list[el_lists];
                            if (spinup) 
                            {  
                                if (t_spin_sl.size()>1) 
                                {
                                    if (t_spin_sl.size()!=list_length) throw exception("Size of t_spin_sl is not of size of sitelist!"); 
                                    
                                    List tspin_listXX(t_spin_sl); 
                                    tspin_list_=tspin_listXX[el_lists];
                                } 
                                else //only one number -> all sites have same size
                                {
                                    List tspin_listXX(t_spin_sl); 
                                    tspin_list_=tspin_listXX[0];
                                } 
                            }
               
                        }
                   
                        NumericMatrix A_sl_(nr_pools,nr_pools); 
                        NumericMatrix Cin_sl_(tsim_list_,nr_pools); 
                        NumericMatrix Nin_sl_ = Cin_sl_/10;
                        NumericVector v_list (tsim_list_*nr_pools,1.0);
                        NumericMatrix xi_sl_(tsim_list_,nr_pools,v_list.begin());
                        NumericVector C0_sl_ (nr_pools); 
                        NumericVector N0_sl_ = C0_sl_/10;
                        NumericVector site_sl_ (sitecol);
                        NumericVector CN_spin_sl_(nr_pools);
                        const char* sitelist_ = "000";
                        NumericMatrix meas_data_sl_; 
                        NumericMatrix env_in_sl_(tsim_list_,2,v_list.begin());
                        List Cin_wood_sl_;
                        List Nin_wood_sl_;
                        List Cin_u_list_;
                        List Nin_u_list_;
                        List xi_u_list_;
                        List Cin_wood_u_list_;
                        List Nin_wood_u_list_;
                        List Cin_wood_u_;
                        List Nin_wood_u_;
                        NumericVector wood_diam_sl_;  
                        double dpmrpm;
                  
                        int unc_length=1;
                        if (theta_n_unc>1 && theta_unc.isNotNull()) unc_length=theta_n_unc;
                        List listout_list_unc; // total list with uncertainties includes all listout_lists
                        List listout_unc; // total list with uncertainties 
        
                        if (!multisite)
                        {
                            String c25="";   
                            if (wood)
                            {
                                if (contains_list(Cin_wood_,0))
                                {
                                    cin_uncertain=true;
                                    Cin_wood_u_= Cin_wood_;
                                    unc_length = Cin_wood_.size();                   
                                }
                                if (calcN==true && contains_list(Nin_wood_,0))
                                {
                                    nin_uncertain=true;
                                    Nin_wood_u_= Nin_wood_;
                                    if (Nin_wood_.size() != unc_length && unc_length>1) c25 +=("When using uncertainties for Nin_wood and Cin_wood, both inputs must have same length! ");
                                    if (unc_length==1) unc_length = Nin_wood_.size(); //wenn keine Uncertainties für C, dann von N abhängig 
                                }
                            }
                            else
                            {  
                                if (TYPEOF(Cin) == VECSXP) 
                                {
                                    cin_uncertain=true;
                                    unc_length = Cin_u_.size();
                                }
                                if (calcN==true && TYPEOF(Nin) == VECSXP) 
                                {
                                    nin_uncertain=true;
                                    if (Nin_u_.size() != unc_length && unc_length>1) c25 +=("When using uncertainties for Nin and Cin, both inputs must have same length! ");
                                    if (unc_length==1) unc_length = Nin_u_.size(); //wenn keine Uncertainties für C, dann von N abhängig 
                                }                
                            }
                            if (TYPEOF(xi) == VECSXP) 
                                {
                                    xi_uncertain=true;
                                    if (xi_u_.size() != unc_length && unc_length>1) c25 +=("When using uncertainties for xi, and Cin or Nin, all inputs must have same length! ");
                                    if (unc_length==1) unc_length = xi_u_.size(); //wenn keine Uncertainties für C und N, dann von xi abhängig 
                                }
                            if (strlen(c25.get_cstring())>10) throw exception(c25.get_cstring());  
                            
                        }   
   
                        if (multisite) 
                        {
                            if (sitelist.isNotNull()) 
                            {   
                                List sitelistXX(sitelist);
                                sitelist_ = sitelistXX[el_lists];
                            }
                
                            ///////////////////////////////////////
                            //data type check for multisite
                            ///////////////////////////////////////
                     
                            List listCin(Cin_sl);
                            List listNin(Nin_sl);
                            List listxi(xi_sl);
                            List listenv_in(env_in_sl);
                            List listmeas_data(meas_data_sl);
                            List listsite(site_sl);              
                            String c3="";    
                            String c3b=""; 
                            String c3c=""; 
                            if (!wood)
                            {
                                if (Cin_sl.isNotNull()) 
                                {
                                    if (!Rf_isNumeric(listCin[el_lists])) c3 +=("Cin_sl contains elements that are not of type double! Please use lists of matrices!");  
                                }
                                if (Nin_sl.isNotNull()) 
                                {
                                    if (!Rf_isNumeric(listNin[el_lists]) && calcN) c3 +=("Nin_sl contains elements that are not of type double! Please use lists of matrices!");   
                                }
                            }
                            if (xi_sl.isNotNull()) 
                            {
                                if (!Rf_isNumeric(listxi[el_lists])) c3 +=("xi_sl contains elements that are not of type double! Please use lists of matrices!");   
                            }
                            if (calcC0 || calcN0)
                            {   
                                if (!Rf_isNumeric(listmeas_data[el_lists])) c3 += ("meas_data_sl contains elements that are not of type double! Please use lists of matrices!");    
                            }
                            if (model_def)
                            {
                                if (!Rf_isNumeric(listenv_in[el_lists])) c3 +=("env_in_sl contains elements that are not of type double");
                                if (!someyasso) //other models but yasso
                                {
                                    if (!Rf_isNumeric(listsite[el_lists])) c3 +=("site_sl contains elements that are not of type double");
                                }
                            }
                            //part renewed for v1.1.0, wood_sz now also re-defined when !calcN
                            if (wood)
                            {
                                List listCin_w(Cin_wood_sl);
                                List list_Cin_wood(listCin_w[el_lists]);
                                
                                if (!contains_list(list_Cin_wood,0)) //contains list -> uncertain, do not check here
                                {   
                                    int Cin_wood_size = list_Cin_wood.size();
                                    wood_sz = Cin_wood_size;
                                    if (calcN)
                                    {
                                        List listNin_w(Nin_wood_sl);
                                        List list_Nin_wood(listNin_w[el_lists]);
                                        int Nin_wood_size = list_Nin_wood.size();
                                        if (Cin_wood_size !=Nin_wood_size) throw exception("Cin_wood and Nin_wood must have the same number of wood layers!"); 
                                    }
                                }
                            }
                            if (A_sl.isNotNull()) A_sl_ = generate_input_matrix(A_sl, el_lists,list_length,"A_sl",nr_pools,0);
                            else A_sl_=A_;
                            if (Cin_sl.isNotNull()) 
                            {   
                                if (contains_list(Cin_sl,el_lists)) 
                                {
                                    cin_uncertain=true;
                                    Cin_u_list_ = generate_input_list(Cin_sl, el_lists,list_length,"Cin_sl","site"); 
                                    unc_length = Cin_u_list_.size();
                                }
                                else 
                                {
                                    cin_uncertain=false;
                                    Cin_sl_ = generate_input_matrix(Cin_sl, el_lists,list_length,"Cin_sl",nr_pools,0); 
                                }
                            }
                            if (calcN==true && Nin_sl.isNotNull())  
                            {     
                                if (contains_list(Nin_sl,el_lists)) 
                                {
                                    nin_uncertain=true;
                                    Nin_u_list_ = generate_input_list(Nin_sl, el_lists, list_length,"Nin_sl","site"); 
                                    if (Nin_u_list_.size() != unc_length && unc_length>1) c3c +=("When using uncertainties for Nin and Cin, both inputs must have same length! ");
                                    if (unc_length==1) unc_length = Nin_u_list_.size(); //wenn keine Uncertainties für C, dann von N abhängig 
                                }
                                else
                                {
                                    nin_uncertain=false;
                                    Nin_sl_ = generate_input_matrix(Nin_sl, el_lists,list_length,"Nin_sl",nr_pools,tsim_list_); 
                                }
                            }

                            if (C0_sl.isNotNull()) C0_sl_ = generate_input_vector(C0_sl, el_lists,list_length,"C0_sl",nr_pools);
                            if (N0_sl.isNotNull()) N0_sl_ = generate_input_vector(N0_sl, el_lists,list_length,"N0_sl",nr_pools);
                            if (CN_spin_sl.isNotNull() && spinup) CN_spin_sl_ = generate_input_vector(CN_spin_sl, el_lists,list_length,"CN_spin_sl",nr_pools);
                            if (site_sl.isNotNull()) site_sl_ = generate_input_vector(site_sl, el_lists,list_length,"site_sl",0);

                            if (Cin_wood_sl.isNotNull()) 
                            {   
                                Cin_wood_u_list_ = generate_input_list(Cin_wood_sl, el_lists,list_length,"Cin_wood_sl","site"); 
                                if (contains_list(Cin_wood_u_list_,1)) 
                                {
                                    cin_uncertain=true;
                                    unc_length = Cin_wood_u_list_.size(); 
                                }
                                else 
                                {
                                    cin_uncertain=false;  
                                    Cin_wood_sl_= generate_input_list(Cin_wood_sl, el_lists,0,"Cin_sl","site"); 
                                }
                            } 
                            if (calcN==true && Nin_wood_sl.isNotNull()) 
                            {
                                Nin_wood_u_list_ = generate_input_list(Nin_wood_sl, el_lists,list_length,"Nin_wood_sl","site");
                                if (contains_list(Nin_wood_u_list_,0)) 
                                {
                                    nin_uncertain=true;
                                    if (Nin_wood_u_list_.size() != unc_length && unc_length>1) c3c +=("When using uncertainties for Nin_wood and Cin_wood, both inputs must have same length! ");
                                    if (unc_length==1) unc_length = Nin_wood_u_list_.size(); //wenn keine Uncertainties für C, dann von N abhängig 
                                    if (strlen(c3b.get_cstring())>10)
                                    {   
                                        String c9999 = sitelist_;
                                        if (multisite) 
                                        {
                                            c3c +=" , Stand: ";
                                            c3c +=c9999;
                                        }
                                    }   
                                }                
                                else 
                                {
                                    nin_uncertain=false;  
                                    Nin_wood_sl_= generate_input_list(Nin_wood_sl, el_lists,0,"Nin_sl","site"); 
                                }
                            }
                  
                            if (xi_sl.isNotNull()) 
                            {   
                                if (contains_list(xi_sl,el_lists)) 
                                {
                                    xi_uncertain=true;
                                    xi_u_list_ = generate_input_list(xi_sl, el_lists,list_length,"xi_sl","site"); 
                                    if (xi_u_list_.size() != unc_length && unc_length>1) c3c +=("When using uncertainties for xi, and Nin or Cin, all inputs must have same length! ");
                                    if (unc_length==1) unc_length = xi_u_list_.size(); //wenn keine Uncertainties für C und N, dann von xi abhängig 
                                }
                                else 
                                {
                                    xi_uncertain=false;
                                    xi_sl_ = generate_input_matrix(xi_sl, el_lists,list_length,"xi_sl",nr_pools,tsim_list_); 
                                }
                            }    
                            if (RothC_dpmrpm_arma.n_elem<(list_length) && model.get_cstring()==rothc && RothC_dpmrpm_arma.n_elem>1) c3c +=("Fewer RothC_dpmrpm arguments than sites!");
                            if (any(RothC_dpmrpm_arma<=0)) c3c +=("Some RothC_dpmrpm arguments are <= 0!");
                            if (strlen(c3c.get_cstring())>10) throw exception(c3c.get_cstring()); 
                            
                            if (wood_diam_sl.isNotNull()) wood_diam_sl_ = generate_input_vector(wood_diam_sl, el_lists,list_length,"wood_diam_sl",0);
                            if (CN_fast_init_sl.isNotNull()) 
                            {   
                                List CN_fast_init_slXX(CN_fast_init_sl); 
                                CNfast=CN_fast_init_slXX[el_lists];
                            }  
                            if (CN_bio_sl.isNotNull()) 
                            {   
                                List CN_bio_slXX(CN_bio_sl); 
                                CNbio=CN_bio_slXX[el_lists];
                            }   
                            if (meas_data_sl.isNotNull()) meas_data_sl_ = generate_input_matrix(meas_data_sl, el_lists,list_length,"meas_data_sl",0,tsim_list_);
                            if (env_in_sl.isNotNull()) 
                            {
                                if (timediv!=1) env_in_sl_ = generate_input_matrix(env_in_sl, el_lists,list_length,"env_in_sl",0,tsim_list_); 
                                if (timediv==1) env_in_sl_ = generate_input_matrix(env_in_sl, el_lists,list_length,"env_in_sl",0,tsim_list_*12);
                            }
                        }  

                        arma::mat xi_arma = as<arma::mat>(xi_); //here defined, filled later
                        if (multisite) 
                        {
                            A_arma = as<arma::mat>(A_sl_);
                            xi_arma = as<arma::mat>(xi_sl_);
                        }
                          
                        //change order of wood unc lists
                        if(cin_uncertain && multisite && Cin_wood_sl.isNotNull()) //C uncertain & list & wood
                        {
                            List in_listX(Cin_wood_u_list_);
                            wood_sz=in_listX.size();
                            List InnerList = in_listX[0];  //3rd level list
                            unc_length=InnerList.size(); 
                            Cin_wood_u_list_ = change_list_order(Cin_wood_u_list_,"Cin_wood_sl",sitelist_,el_lists+1); //for wood_unc need to change order because unc loop must be out of wood loop
                            if (calcN==true && cin_uncertain) Nin_wood_u_list_ = change_list_order(Nin_wood_u_list_,"Nin_wood_sl",sitelist_,el_lists+1);  
                        }
                        if(cin_uncertain && !multisite && Cin_wood.isNotNull()) //C uncertain & no list & wood
                        {
                            List in_listX(Cin_wood_u_);
                            wood_sz=in_listX.size();
                            List InnerList = in_listX[0]; //2nd level list
                            unc_length=InnerList.size(); 
                            Cin_wood_u_ = change_list_order(Cin_wood_u_,"Cin_wood","",el_lists+1); //for wood_unc need to change order because unc loop must be out of wood loop
                            if (calcN==true && cin_uncertain) Nin_wood_u_ = change_list_order(Nin_wood_u_,"Nin_wood","",el_lists+1);
                        }
                        
                        //matrix for saving wood slices - only filled when ts==1 and unc==0
                        arma::mat wood_xi_slice(wood_sz, nr_pools,arma::fill::ones);
     
                        if (model_def && !cin_uncertain && !nin_uncertain && Rf_isMatrix(theta)) unc_length=thetamat_.nrow(); //model defined but only theta defines uncertainties
              
                        for (int uncert=0; uncert<unc_length; uncert++)
                        { 
                                                unsigned int nr_p_unsi = nr_pools;
                                                if (multisite) 
                                                {
                                                    tsim=tsim_list_;
                                                    if (spinup) tspin=tspin_list_;
                                                }
                                                unsigned int tseqlength_unsi = tsim;
                                                unsigned int tseqlength_unsi_m = tseqlength_unsi;
                                                if (timediv==1) tseqlength_unsi_m=tseqlength_unsi*12; //monthly vector length for calculating xi
                                                int tseqlength = 0;
                                                if (spinup) tseqlength = tspin;
                                                else tseqlength = tsim;
                                                
                                                if(cin_uncertain && multisite && Cin_sl.isNotNull()) //C uncertain & list & no wood
                                                {
                                                    check_list(Cin_u_list_, "Cin_sl", 0, sitelist_, uncert );
                                                    Cin_sl_ = generate_input_matrix(Cin_u_list_,uncert,unc_length,"At least one uncertainty layer of Cin_sl",nr_pools,tsim_list_); 
                                                }
                                                if(cin_uncertain && !multisite && Cin.isNotNull()) //C uncertain & no list & no wood  
                                                {
                                                    check_list(Cin_u_, "Cin", 0, "", uncert );
                                                    Cin_ = generate_input_matrix(Cin_u_,uncert,unc_length,"At least one uncertainty layer of Cin",nr_pools,tsim_list_);
                                                }
                                                if(nin_uncertain && multisite && Nin_sl.isNotNull()) //N uncertain & list & no wood
                                                {
                                                    check_list(Nin_u_list_, "Nin_sl", 0, sitelist_, uncert );
                                                    Nin_sl_ = generate_input_matrix(Nin_u_list_,uncert,unc_length,"At least one uncertainty layer of Nin_sl",nr_pools,tsim_list_); 
                                                }
                                                if(nin_uncertain && !multisite && Nin.isNotNull()) //N uncertain & no list & no wood  
                                                {
                                                    check_list(Nin_u_, "Nin", 0, "", uncert );
                                                    Nin_ = generate_input_matrix(Nin_u_,uncert,unc_length,"At least one uncertainty layer of Nin",nr_pools,tsim_list_);
                                                }            
                                                if(cin_uncertain && multisite && Cin_wood_sl.isNotNull()) //C uncertain & list & wood
                                                {
                                                    Cin_wood_sl_ = generate_input_list(Cin_wood_u_list_,uncert,unc_length,"Cin_wood_sl","uncertainties"); 
                                                    //check_list(Cin_wood_sl_, "C", 1, sitelist_, uncert ); //wood checked already with change_list_order
                                                }
                                                if(cin_uncertain && !multisite && Cin_wood.isNotNull()) //C uncertain & no list & wood
                                                {
                                                    Cin_wood_ = generate_input_list(Cin_wood_u_,uncert,unc_length,"Cin_wood","uncertainties"); 
                                                    //check_list(Cin_wood_, "C", 1, "", uncert );
                                                }
                                                if(nin_uncertain && multisite && Nin_wood_sl.isNotNull()) //N uncertain & list & wood 
                                                {
                                                    Nin_wood_sl_ = generate_input_list(Nin_wood_u_list_,uncert,unc_length,"Nin_wood_sl","uncertainties"); 
                                                    //check_list(Nin_wood_sl_, "N", 1, sitelist_, uncert );
                                                }
                                                if(nin_uncertain && !multisite && Nin_wood.isNotNull()) //N uncertain & no list & wood                                                  
                                                {
                                                    Nin_wood_ = generate_input_list(Nin_wood_u_,uncert,unc_length,"Nin_wood","uncertainties"); 
                                                    //check_list(Nin_wood_, "N", 1, "", uncert );
                                                }  
                                                if(xi_uncertain && multisite && xi_sl.isNotNull()) //xi uncertain & list 
                                                {
                                                    check_list(xi_u_list_, "xi_sl", 0, sitelist_, uncert );
                                                    xi_sl_ = generate_input_matrix(xi_u_list_,uncert,unc_length,"At least one uncertainty layer of xi_sl",nr_pools,tsim_list_); 
                                                } 
                                                if(xi_uncertain && !multisite && xi.isNotNull()) //xi uncertain & no list 
                                                {
                                                    check_list(xi_u_, "xi", 0, "", uncert );
                                                    xi_ = generate_input_matrix(xi_u_,uncert,unc_length,"At least one uncertainty layer of xi",nr_pools,tsim_list_);
                                                }
                                
                                                // Rccp to RccpArmadillo
                                                arma::vec C0_arma = as<arma::vec>(C0_);
                                                arma::vec C0_fracts_arma = as<arma::vec>(C0_fracts_);
                                                arma::vec N0_arma = as<arma::vec>(N0_);
                                                arma::vec pt_arma = as<arma::vec>(pooltypes_);
                                                arma::vec CN_spin_arma = as<arma::vec>(CN_spin_);
                                                if ((!model_def) && (pooltypes.isNull())) pt_arma.fill(0);
                                                    
                                                arma::mat Cin_arma = as<arma::mat>(Cin_);
                                                arma::mat Nin_arma = as<arma::mat>(Nin_);
                                                arma::mat xi_arma_generated = as<arma::mat>(xi_);
                                                
                                                if (xi_uncertain && !model_def) xi_arma = xi_arma_generated;  //else xi is calculated 

                                                int cubecol=1;
                                                int cuberow=1;
                                                int wood_size=1;

                                                //////////////////////////////////////////////////////////////////////////////////////////////////////////
                                                // input verifications wood
                                                //////////////////////////////////////////////////////////////////////////////////////////////////////////                                               
                                                if(wood)
                                                {
                                                    if (multisite) 
                                                    { 
                                                        wood_size = Cin_wood_sl_.size();                                             
                                                        if (wood_size!=wood_diam_sl_.size()) checkwooddiamlist(wood_diam_sl_.size(), wood_size, uncert+1, el_lists+1,cin_uncertain,multisite,1);
                                                        if (calcN && Nin_wood_sl_.size() !=wood_diam_sl_.size()) checkwooddiamlist(wood_diam_sl_.size(), Nin_wood_sl_.size(), uncert+1, el_lists+1,nin_uncertain,multisite,2);
                                                        NumericMatrix Cin_wood_mat = Cin_wood_sl_[0];  
                                                        cubecol = Cin_wood_mat.ncol();
                                                        cuberow = Cin_wood_mat.nrow(); 
                                                        if (wood_diam_sl.isNull())  
                                                        {
                                                            if (wood_diam.isNull())
                                                            {  
                                                                NumericVector wood_diam_def(wood_size, 0.0); //length wood_size, filled with zeros
                                                                wood_diam_sl_ = wood_diam_def; 
                                                            }
                                                            else wood_diam_sl_ = wood_diam_;
                                                        }     
                                                    }
                                                    else
                                                    {
                                                        wood_size = Cin_wood_.size();
                                                        if (wood_size!=wood_diam_.length()) checkwooddiamlist(wood_diam_.length(), wood_size, uncert+1, 0,cin_uncertain,multisite,1);
                                                        if (calcN && Nin_wood_.size() !=wood_diam_.length()) checkwooddiamlist(wood_diam_.length(), Nin_wood_.size(), uncert+1, 0,nin_uncertain,multisite,2);
                                                        NumericMatrix Cin_wood_mat = Cin_wood_[0];
                                                        cubecol = Cin_wood_mat.ncol();
                                                        cuberow = Cin_wood_mat.nrow();
                                                        
                                                        if (wood_diam.isNull())  
                                                        {
                                                            NumericVector wood_diam_def(wood_size, 0.0);
                                                            wood_diam_ = wood_diam_def; 
                                                        }
                                                    }
                                                    
                                                    if (multisite) 
                                                    {
                                                        checkwooddims(wood_size, Cin_wood_sl_, multisite, cin_uncertain, el_lists+1, uncert+1,tseqlength_unsi,nr_pools,1);
                                                        if (calcN) checkwooddims(wood_size, Nin_wood_sl_, multisite, nin_uncertain, el_lists+1, uncert+1,tseqlength_unsi,nr_pools,2);
                                                    }
                                                    else 
                                                    {                                                         
                                                        checkwooddims(wood_size, Cin_wood_,multisite, cin_uncertain, el_lists+1, uncert+1,tseqlength_unsi,nr_pools,1); 
                                                        if (calcN) checkwooddims(wood_size, Nin_wood_, multisite, nin_uncertain, el_lists+1, uncert+1,tseqlength_unsi,nr_pools,2);
                                                    }
                                                } //if wood

                                                arma::cube Cin_wood_arma(cuberow, cubecol, wood_size);
                                                arma::cube Nin_wood_arma(cuberow, cubecol, wood_size);  
                                                arma::mat Nin_prev_wood(cuberow, cubecol) ; //to store input sums of previous Nin
                                                     
                                                arma::vec wood_diam_arma = as<arma::vec>(wood_diam_);
                
                                                int wood_length=1;
                                                if (wood) wood_length = Cin_wood_arma.n_slices;             
                                                arma::mat meas_data = as<arma::mat>(meas_data_);
                                                arma::mat env_in_arma = as<arma::mat>(env_in_);
                                                arma::vec site_arma = as<arma::vec>(site_);
                                                arma::vec theta_arma = as<arma::vec>(theta_);
                                                
                                                // theta randomization if list (matrix) of parameters or (when theta_unc) from distribution
                                                if (Rf_isMatrix(theta) )
                                                {
                                                    if ( cin_uncertain || nin_uncertain )
                                                    {
                                                        NumericVector theta_row = floor(runif(1)*thetamat_.nrow());
                                                        theta_arma = thetamat_(theta_row(0),_);
                                                    }
                                                    else theta_arma = thetamat_(uncert,_); //only theta is responsible for uncertainties               
                                                }
                                                else if (unc_length>1 && theta_unc.isNotNull())
                                                {
                                                    arma::vec theta_factors_arma(thetacol,arma::fill::ones);
                                                    for(int sd=0; sd<thetacol; sd++) theta_factors_arma(sd)=rnorm(1,1,thetafac_[sd])[0]; //normal distribution around 1 with sd=thetafac_[sd]
                                                    theta_arma = theta_arma%theta_factors_arma;
                                                }

                                                if (multisite) 
                                                {
                                                    C0_arma = as<arma::vec>(C0_sl_);
                                            
                                                    Cin_arma = as<arma::mat>(Cin_sl_);
                                                    Nin_arma = as<arma::mat>(Nin_sl_);
                                                    if (spinup) 
                                                    {
                                                        if (CN_spin_sl.isNull()) CN_spin_arma = as<arma::vec>(CN_spin_);
                                                        else CN_spin_arma = as<arma::vec>(CN_spin_sl_);
                                                    }
                                                    xi_arma_generated = as<arma::mat>(xi_sl_); 
                                    
                                                    if (xi_uncertain && !model_def) xi_arma = xi_arma_generated;  //else xi is calculated 

                                                    meas_data = as<arma::mat>(meas_data_sl_);
                                                    env_in_arma = as<arma::mat>(env_in_sl_);
                                                    site_arma = as<arma::vec>(site_sl_); 
                                                    wood_diam_arma = as<arma::vec>(wood_diam_sl_);
                                                    if (wood)
                                                    {
                                                        for(int list_elem=0; list_elem<Cin_wood_sl_.size(); list_elem++) // Cin_wood_sl ist eine Liste von Holzinput in einer Liste von Standorten, Cin_wood_sl_ ist ein Standort
                                                        { 
                                                            
                                                            NumericMatrix Cin_wood_elem =Cin_wood_sl_[list_elem];
                                                            Cin_wood_arma.slice(list_elem) = as<arma::mat>(Cin_wood_elem);
                                                            if (calcN) 
                                                            {                  
                                                                NumericMatrix Nin_wood_elem =Nin_wood_sl_[list_elem];
                                                                Nin_wood_arma.slice(list_elem) = as<arma::mat>(Nin_wood_elem);
                                                            }
                                                        }
                                                        if (calcN) Nin_prev_wood = sum(Nin_wood_arma,2) ; //to store input sums of previous Nin
                                                     
                                                    }
                                                }
                                                else
                                                {
                                                    if (wood)
                                                    {
                                                        for(int list_elem=0; list_elem<Cin_wood_.size(); list_elem++)
                                                        {      
                                                            NumericMatrix Cin_wood_elem =Cin_wood_[list_elem]; 
                                                            Cin_wood_arma.slice(list_elem) = as<arma::mat>(Cin_wood_elem);
                                                            if (calcN) 
                                                            { 
                                                                NumericMatrix Nin_wood_elem =Nin_wood_[list_elem];
                                                                Nin_wood_arma.slice(list_elem) = as<arma::mat>(Nin_wood_elem);
                                                            }
                                                        }  
                                                        if (calcN) Nin_prev_wood = sum(Nin_wood_arma,2) ; //to store input sums of previous Nin
                                                    }
                                                }  

                                                //////////////////////////////////////////////////////////////////////////////////////////////////////////
                                                //input verification
                                                //////////////////////////////////////////////////////////////////////////////////////////////////////////    
                                    
                                                if (el_lists==0 && uncert==0)
                                                {
                                                    if (model.get_cstring()!=ctool && model.get_cstring()!=ctool_o && CTool_input_raw) Rcout << "Warning! CTool_input_raw only has effect on C-Tool runs"  << " \n";
                                                    if (model_def && multisite && xi_sl.isNotNull() )  Rcout << "Warning! xi_sl input has no effect. A model has been chosen."  << " \n";
                                                    if (model_def && multisite==false && xi.isNotNull() )  Rcout << "Warning! xi input has no effect. A model has been chosen."  << " \n";
                                                    if (model_def && multisite && A_sl.isNotNull() )  Rcout << "Warning! A_sl input has no effect. A model has been chosen."  << " \n";
                                                    if (model_def && multisite==false && A.isNotNull() )  Rcout << "Warning! A input has no effect. A model has been chosen."  << " \n";
                                                    if (C0_fracts.isNotNull() && model.get_cstring()==ctool_o) Rcout << "Warning! C-Tool-org usually runs without C0. "  << " \n";
                                                    if (C0_fracts.isNull() && model.get_cstring()==ctool_o && calcC0==true) Rcout << "Warning! calcC0 has no effect unless you also define C0_fracts, since there are no standard C0_fracts for C-Tool-org."  << " \n";
                                                    if (multisite && C0_sl.isNotNull() && model.get_cstring()==ctool_o) Rcout << "Warning! C-Tool-org usually runs without C0."  << " \n";
                                                    if (C0.isNotNull() && multisite==false && model.get_cstring()==ctool_o) Rcout << "Warning! C-Tool-org usually runs without C0."  << " \n";
                                                    if (model_def && model.get_cstring()!=ctool_o && C0_fracts.isNotNull() )  Rcout << "Warning! Changing C0_fracts for predefined model."  << " \n";
                                                    if (calcC0==false && calcN0==true) Rcout << "Warning! N0 is to be calculated but C0 is not. Using given C0 input and measured N0 to calculate initial CN which might be far-fetched."  << " \n";
                                                }                                    

                                                String s1 ="";
                                                String s2 ="";
                                                String s3 ="";
                                                String s4 ="";
                                                String s5 ="";
                                                String s6 ="";
                                                String s7 ="";
                                                String s8 ="";
                                                String s9 ="";
                                                String s99 =tsim;
                                                String s999 = nr_pools;     
                                                
                                                if (el_lists==0 && uncert==0)
                                                {
                                                    if (calcC0) //hier!! C0_fracts nicht nur bei calcC0, sondern auch wenn C0_arma.n_elem == 1 
                                                    {
                                                        String s00 ="";
                                                        if (meas_data.n_cols<2)   s00 +=("Measured data input must have 2 columns: time, C"); 
                                                        if (strlen(s00.get_cstring())>10) throw exception(s00.get_cstring());
                                                    }
                                                    if (calcC0 || C0_arma.n_elem == 1) 
                                                    {
                                                        String s000 ="";
                                                        if (C0_fracts.isNull() && !model_def) s000 +=("When not using a predefined model, calculation of C0 requires definition of C0_fracts!");
                                                        if (C0_fracts_arma.n_elem != nr_p_unsi) s000 +=(" Length of C0_fracts must match number of pools! ");  
                                                        if ( (someyasso || !model_def) && ((sum(C0_fracts_arma)<0.9999) || (sum(C0_fracts_arma)>1)) ) s000 +=(" Sum of C0_fracts must be 1! ");  
                                                        if (( model.get_cstring()==rothc && C0_fracts.isNotNull()) && ((sum(C0_fracts_arma)<0.9999) || (sum(C0_fracts_arma)>1)) )s000 +=(" Sum of C0_fracts must be 1! ");  
                                                        if ( model.get_cstring()==ctool )
                                                        {
                                                            if ( (C0_fracts_arma(0)+C0_fracts_arma(1)+C0_fracts_arma(2)<0.9999) || (C0_fracts_arma(0)+C0_fracts_arma(1)+C0_fracts_arma(2)>1))  s000 +=(" Sum of topsoil of C0_fracts (first 3 numbers) must be 1 (with C-Tool)! ");  
                                                        }
                                                        if (any (C0_fracts_arma <0) || any (C0_fracts_arma>1)) s000 +=(" Unrealistic C0_fracts! ");  
                                                        if (strlen(s000.get_cstring())>10) throw exception(s000.get_cstring());
                                                    }

                                                    if (calcN0)
                                                    {
                                                        String s0 ="";
                                                        if (meas_data.n_cols<3)   s0 +=("Measured data input must have 3 columns: time, C, N ");   
                                                        if (calcN0 && !calcN) s0 +=("It makes no sence to calculate N0 without modelling N. Either set calcN0 to FALSE or calcN to TRUE ");
                                                        if (all(C0_arma <=0) && !calcC0) s0 +=("C0 of ZERO not allowed when calculating N0"); 
                                                        if (!model_def) 
                                                        {
                                                            if (pt_arma.is_zero())   s0 +=(" When calculating N0 and model is not defined 'pooltypes' must be defined!");
                                                            if (pt_arma.n_elem != nr_p_unsi) s0 +=(" 'pooltypes' must match number of pools! Did you define it correctly? Number of pools equals the dimensions of 'A'. Did you define it correctly?"); 
                                                            if (all(pt_arma !=3)) s0 +=(" 'pooltypes' must contain at least one topsoil resistent/inert pool (=3)!"); 
                                                            if ( (any(pt_arma <0)) || (any(pt_arma >6)) ) s0 +=(" Only values between 1 to 6 are allowed for 'pooltypes'!"); 
                                                        }
                                                        if (strlen(s0.get_cstring())>10) throw exception(s0.get_cstring());
                                                    } 

                                                    if (A_arma.n_rows != A_arma.n_cols)  s1 +=(" Matrix A must be quadratic! ");

                                                    if (!calcC0)
                                                    {        
                                                        if (C0_arma.n_elem != nr_p_unsi && C0_arma.n_elem != 1) s1 +=("Length of C0 dimensions must match number of pools, or must be a scalar and pool values will be calculated by given or model-specific fractions!");   
                                                        if (C0_arma.n_elem == 1 && nr_p_unsi!=1 && (C0_fracts.isNull() && model.get_cstring()!=rothc)) s1 +=("Need to specify CO fractions when C0 is given as scalar!");
                                                    }
                                                    if (calcN && !calcN0)
                                                    {
                                                        if (N0_arma.n_elem != nr_p_unsi) s1 +=(" Length of N0 dimensions must match number of pools! ");  
                                                    }
                                                    if (!model_def && xi_arma.n_cols != nr_p_unsi) s1 +=(" Number of Columns of Xi input data frame must match number of pools! "); 
                                                
                                                    if (strlen(s1.get_cstring())>10)
                                                    {
                                                        s1 +=("Number of pools defined as: ");
                                                        s1 +=s999;  
                                                        s1 +=(". Did you correctly define matrix A ?");
                                                    }   
                                                    
                                                    if (strlen(s2.get_cstring())>10)
                                                    {
                                                        s2 +=("Number of time steps defined as: ");
                                                        s2 +=s99;
                                                        s1 +=s2;
                                                    }          
                                                        
                                                    if ((!calcN) && (calcNbalance)) 
                                                    {   
                                                        s3 +=(" Can't calculate N balance without N! ");
                                                        s1+=s3;
                                                    }
                                    
                                                    if (any(C0_arma <0)) s4 +=(" Negative values in C0 are not allowed! ");   
                                                    if (any(N0_arma <0)) s4 +=(" Negative values in N0 are not allowed! ");        
                                                    if (any(vectorise(xi_arma) <0)) s4 +=(" Negative environmental factors are not allowed! As always! ");   
                                                    
                                                    if (strlen(s4.get_cstring())>10) s1+=s4;   
                                                    
                                                    if (model_def && someyasso && env_in_arma.n_cols != 2 ) s5 +=(" Data frame for xi calculation (env_in) of Yasso needs two columns: T & p! ");   
                                                    if (model_def && yasso15up && theta_arma.n_elem != 30 ) s5 +=(" Parameter vector (theta) of Yasso > version 15 must contain 30 numbers!"); 
                                                    if (model_def && model.get_cstring()==rothc && any(theta_arma < 0) ) s5 +=(" Parameter vector (theta) of RothC must not contain negative values!"); 
                                                    if (model_def && model.get_cstring()==rothc && env_in_arma.n_cols != 4 ) s5 +=(" Data frame for xi calculation (env_in) of RothC needs four columns: T, p, ETP and information on crop (0 or 1)! ");  
                                                    //if (model_def && (model.get_cstring()==ctool || model.get_cstring()==ctool_o) && tsteps.get_cstring()==annually) s5 +=(" xi calculation (env_in) of C-Tool at the moment only for monthly resolution at minimum. ");
                                                    if (strlen(s5.get_cstring())>10) s1+=s5;  
                                                    if (wood)
                                                    {
                                                        if (any(wood_diam_arma < 0) ) 
                                                        {
                                                            s6 +=(" Negative wood diameters are not allowed!");
                                                        }  
                                                        if (strlen(s6.get_cstring())>10) s1+=s6;
                                                    }
                                                    if (model_def)
                                                    {
                                                        if (model.get_cstring()==rothc && env_in_arma.n_cols!=4) s8 +=(" Modelling with RothC requires 'env_in' with 4 columns. Please define env_in correctly (T, p, ETP and crop info)! ");
                                                        if ((model.get_cstring()==ctool || model.get_cstring()==ctool_o )  && env_in_arma.n_cols!=1) s8 +=(" Modelling with C-Tool requires 'env_in' with 1 column of T. Please define env_in correctly! ");
                                                        if (someyasso  && env_in_arma.n_cols!=2) s8 +=(" Modelling with Yasso requires 'env_in' with 2 columns. Please define env_in correctly (T, p)! ");
                                                        if (strlen(s8.get_cstring())>10) throw exception(s8.get_cstring());
                                                        
                                                        if (model.get_cstring()==rothc) 
                                                        {
                                                                if (site_arma.n_elem<3) s7 +=(" Need site information when using pre-defined RothC model definitions. Please define sample depth, clay content (0 if unknown), black sand status and initial CN (0 if unknown) of site! ");
                                                                else
                                                                {   if (site_arma(0)<=0) s7 +=(" Sample depth must be a positive value! Did you correctly define 'site'? ");
                                                                    if (site_arma(1)<0) s7 +=(" Clay content must not be negative! ");
                                                                    if (site_arma(2)!=0 && site_arma(2)!=1) s7 +=(" Black sand status must be either 0 or 1!");
                                                                    if (site_arma(3)<0) s7 +=(" CN ratio must not be negative (please set to 0 if unknown)!");
                                                                    if (site_arma(3)==0 && calcN0==false && site_arma(2)==1 && C0.isNull() && N0.isNull()) s7 +=(" When going for Springob method, if CN is not defined it must be calculated! Set calcN0 to TRUE or mark black sand status with '0' in site input file to skip the Springob method or define C0 and N0.");
                                                                } 
                                                                if (all(vectorise(env_in_arma)==1.0)) s7 +=(" Need climate and crop information per site when using pre-defined RothC model definitions. Please define env_in (T, p, ETP and crop info)! "); 
                                                                if (!all(env_in_arma.col(3)==1.0 || env_in_arma.col(3)==0.0 || env_in_arma.col(3)==2.0))  s7 +=(" Crop information must be either 0 or 1 or 2! "); 
                                                                if (any(env_in_arma.col(2)<0))  s7 +=(" Evapotranspiration must not be negative! "); 
                                                        }
                                                        if (model.get_cstring()==ctool || model.get_cstring()==ctool_o ) 
                                                        {
                                                                if (site_arma.n_elem<1) s7 +=(" Need site information when using pre-defined C-Tool model definitions. Please define clay content of site! ");
                                                                else if (site_arma(0)<0) s7 +=(" Clay content must not be negative! ");
                                                                if (all(vectorise(env_in_arma)==1.0)) s7 +=(" Need climate information per site when using pre-defined C-Tool model definitions. Please define env_in (T, p)! "); 
                                                        }
                                                        if (someyasso) 
                                                        {
                                                                if (all(vectorise(env_in_arma)==1.0)) s7 +=(" Need climate information per site when using pre-defined Yasso model definitions. Please define env_in (T, p)! "); 
                                                        }
                                                        if (strlen(s6.get_cstring())>10) s1+=s7;
                                                    }
                                                }  
                                
                                                //check Cin and Nin specific stuff for every uncert
                                                if (model_def)
                                                {
                                                    if ( (model.get_cstring()==ctool || model.get_cstring()==ctool_o) && CTool_input_raw &&  Cin_arma.n_cols != 2) s9 +=(" Raw C input data frame must consist of 2 colums, one for top soil, one for subsoil! ");
                                                    if ( (model.get_cstring()==ctool || model.get_cstring()==ctool_o) && CTool_input_raw && calcN && Nin_arma.n_cols != 2) s9 +=(" Raw N input data frame must consist of 2 colums, one for top soil, one for subsoil!  ");  
                                                    if ( !((model.get_cstring()==ctool || model.get_cstring()==ctool_o) && CTool_input_raw) &&  Cin_arma.n_cols != nr_p_unsi) s9 +=(" Number of Columns of C input data frame must match number of pools! ");
                                                    if ( !((model.get_cstring()==ctool || model.get_cstring()==ctool_o) && CTool_input_raw) &&  calcN && Nin_arma.n_cols != nr_p_unsi) s9 +=(" Number of Columns of N input data frame must match number of pools! ");  
                                                }
                                                if (any(vectorise(Cin_arma) <0)) s9 +=(" Negative value in Cin are not allowed! ");      
                                                if (any(vectorise(Nin_arma) <0)) s9 +=(" Negative values in Nin are not allowed! ");   
                                                if (strlen(s9.get_cstring())>10) s1+=s9;
                                                
                                                if (strlen(s1.get_cstring())>10) 
                                                {
                                                    if (multisite)
                                                    {
                                                        String s9999 = sitelist_;
                                                        s1 +=" , Stand: ";
                                                        s1 +=s9999;
                                                    }            
                                                    throw exception(s1.get_cstring());
                                                }

                                                //////////////////////////////////////////////////////////////////////////////////////////////////////////
                                                //A & xi calculation
                                                ////////////////////////////////////////////////////////////////////////////////////////////////////////// 

                                                if (model_def && (uncert==0 || theta_unc.isNotNull() || Rf_isMatrix(theta))) //don't do twice when not uncertain
                                                {
                                                    if (someyasso)
                                                    {  
                                                        if (el_lists==0)
                                                        {
                                                            arma::mat A1(5,5,arma::fill::zeros);
                                                            arma::mat Ap(5,5,arma::fill::zeros);
                                                            A1.diag()=arma::vec({theta_arma(0),theta_arma(1),theta_arma(2),theta_arma(3),theta_arma(4)});        
                                                            Ap.row(0)=arma::rowvec({-1, theta_arma(5), theta_arma(6), theta_arma(7), 0});
                                                            Ap.row(1)=arma::rowvec({theta_arma(8), -1, theta_arma(9), theta_arma(10), 0});
                                                            Ap.row(2)=arma::rowvec({theta_arma(11), theta_arma(12), -1, theta_arma(13), 0});
                                                            Ap.row(3)=arma::rowvec({theta_arma(14), theta_arma(15), theta_arma(16), -1, 0});
                                                            Ap.row(4)=arma::rowvec({theta_arma(17), theta_arma(17), theta_arma(17), theta_arma(17), -1});
                                                            A_arma_yasso_standard=Ap*A1;
                                                        }
                                                        A_arma=A_arma_yasso_standard;
                                                        
                                                        arma::colvec Tvec = env_in_arma.col(0);  
                                                        arma::colvec pvec = env_in_arma.col(1);
  
                                                        int timediv_prec; //when annual mode devide by 12, because environmental input must be monthly
                                                        if (timediv==1) timediv_prec = 12;
                                                        else timediv_prec = timediv;
                                                        int n_yrs = ceil(tseqlength_unsi_m/timediv_prec);
                                                        int maxpos = pvec.n_elem-1;
                                                        for(int ys=0; ys<n_yrs; ys++) 
                                                        {
                                                            double prec_sum;
                                                            prec_sum = sum(  pvec( arma::span(1+timediv_prec*ys,std::min(timediv_prec*(ys+1),maxpos) ) ) )/1000.0; //calculate annual precipitation, leave out first row
                                                            pvec( arma::span(1+timediv_prec*ys,std::min(timediv_prec*(ys+1),maxpos))).fill(prec_sum); //each time step in year gets annual precipitation
                                                            if (ys==0) pvec(0)=prec_sum; // we do not want to extrapolate therefore first value after one month is value at zero
                                                        }  
                                                        if (yasso15up)
                                                        {
                                                            arma::colvec T1 = fT_yasso(Tvec, pvec, theta_arma(18),theta_arma(19),theta_arma(24));// 0.0905980470, -0.0002144096, -1.8089202000); //22,23,28  
                                                            arma::colvec T2 = fT_yasso(Tvec, pvec, theta_arma(20),theta_arma(21),theta_arma(25));// 4.877247e-02, -7.913602e-05, -1.172547e+00); //24,25,29
                                                            arma::colvec T3 = fT_yasso(Tvec, pvec, theta_arma(22),theta_arma(23),theta_arma(26));// 3.518549e-02, -2.089906e-04, -1.253595e+01); //26,27,30                        
                                                            if (timediv==1) //when tsteps annually calculate xi first then take mean of year
                                                            {
                                                                arma::colvec monthly_xi_T1(tseqlength_unsi_m);
                                                                arma::colvec annual_xi_T1(tseqlength_unsi);
                                                                arma::colvec monthly_xi_T2(tseqlength_unsi_m);
                                                                arma::colvec annual_xi_T2(tseqlength_unsi);
                                                                arma::colvec monthly_xi_T3(tseqlength_unsi_m);
                                                                arma::colvec annual_xi_T3(tseqlength_unsi);
                                                                monthly_xi_T1 = T1;
                                                                monthly_xi_T2 = T2;
                                                                monthly_xi_T3 = T3; 

                                                                for(unsigned int i_a=0; i_a<tseqlength_unsi; i_a++) 
                                                                {                  
                                                                    arma::colvec year_vec_T1(12);
                                                                    arma::colvec year_vec_T2(12);
                                                                    arma::colvec year_vec_T3(12);
                                                                    year_vec_T1 = monthly_xi_T1(arma::span(i_a*12,i_a*12+11));
                                                                    annual_xi_T1(i_a) = arma::mean(year_vec_T1);
                                                                    year_vec_T2 = monthly_xi_T2(arma::span(i_a*12,i_a*12+11));
                                                                    annual_xi_T2(i_a) = arma::mean(year_vec_T2);
                                                                    year_vec_T3 = monthly_xi_T3(arma::span(i_a*12,i_a*12+11));
                                                                    annual_xi_T3(i_a) = arma::mean(year_vec_T3);
                                                                }
                                                               
                                                                xi_arma.col(0) = annual_xi_T1;
                                                                xi_arma.col(1) = annual_xi_T1;
                                                                xi_arma.col(2) = annual_xi_T1;
                                                                xi_arma.col(3) = annual_xi_T2;
                                                                xi_arma.col(4) = annual_xi_T3; 
                                                            }
                                                            if (timediv!=1)                                                      
                                                            {
                                                                xi_arma.col(0) = T1;
                                                                xi_arma.col(1) = T1;
                                                                xi_arma.col(2) = T1;
                                                                xi_arma.col(3) = T2;
                                                                xi_arma.col(4) = T3; 
                                                            }
                                                        }
                                                        if (model.get_cstring()==yasso07)
                                                        {
                                                            arma::colvec T1 = fT_yasso(Tvec, pvec, theta_arma(18), theta_arma(19), theta_arma(20));  
                                                            if (timediv==1) //when tsteps annually calculate xi first and then take mean of year
                                                            {
                                                                arma::colvec monthly_xi_T1(tseqlength_unsi_m);
                                                                arma::colvec annual_xi_T1(tseqlength_unsi);
                                                                monthly_xi_T1 = T1;
                                                                
                                                                for(unsigned int i_a=0; i_a<tseqlength_unsi; i_a++) 
                                                                {                  
                                                                    arma::colvec year_vec_T1(12);
                                                                    year_vec_T1 = monthly_xi_T1(arma::span(i_a*12,i_a*12+11));
                                                                    annual_xi_T1(i_a) = arma::mean(year_vec_T1);
                                                                }
                                                                xi_arma.each_col()= annual_xi_T1;
                                                            }
                                                            xi_arma.each_col() = T1;
                                                        }
                                                    }

                                                    if (model.get_cstring()==rothc)
                                                    {
                                                        double clay = site_arma(1);

                                                        double x = 1.67 * (1.85 + 1.6 * exp(-0.0786 * clay));
                                                        double B = 0.46/(x + 1);
                                                        double H = 0.54/(x + 1);

                                                        arma::mat A1(5,5,arma::fill::zeros);
                                                        arma::rowvec ks ({theta_arma(0),theta_arma(1),theta_arma(2),theta_arma(3),theta_arma(4)});
                                                        A1.diag() = -ks;
                                                        
                                                        A1.row(2) = A1.row(2) + B * ks;
                                                        A1.row(3) = A1.row(3) + H * ks;   
                                                        
                                                        A_arma=A1;
                                
                                                        arma::colvec Tvec = env_in_arma.col(0);  
                                                        arma::colvec pvec = env_in_arma.col(1);
                                                        arma::colvec ETPvec = env_in_arma.col(2);
                                                        arma::colvec Cropvec = env_in_arma.col(3);
                                                        arma::colvec RateT = env_in_arma.col(0);
                        
                                                        
                                                        arma::colvec RateW(tseqlength_unsi_m); 
                                                        arma::colvec RateWb(tseqlength_unsi_m); 
                                                        arma::colvec MaxTSMD(tseqlength_unsi_m); 
                                                        arma::colvec MaxTSMDb(tseqlength_unsi_m); //1.0.1
                                                        arma::colvec Mvec(tseqlength_unsi_m); 
                                                        arma::colvec RateC(tseqlength_unsi_m); 
                                                    
                                                        for(unsigned int iw=0; iw<tseqlength_unsi_m; iw++) 
                                                        {
                                                            if (Tvec(iw) > -18.27) RateT(iw) = 47.91/(1 + exp(106.06/(Tvec(iw) + 18.27)));
                                                            //if (Tvec(iw) > -18.3) RateT(iw) = 47.9/(1 + exp(106/(Tvec(iw) + 18.3)));
                                                            else RateT(iw) = 0;
                                                        }
                                                        MaxTSMD.fill (-(20 + 1.3 * clay - 0.01 * pow(clay,2)) * (site_arma(0)/23)/1.8);   
                                                                                      
                                                        //1.0.1
                                                        MaxTSMDb=MaxTSMD;  
                                                        arma::uvec critids2 = find(Cropvec==2); // Find indices without crop, that are to be treated as with crop for calculation of RateWb - microorganisms in soil do not know vegetation cover
                                                        for(const unsigned int& critid2: critids2) MaxTSMDb(critid2)=-(20 + 1.3 * clay - 0.01 * pow(clay,2)) * (site_arma(0)/23);
                                                        
                                                        arma::uvec critids= find(Cropvec==1); // Find indices with crop
                                                        for(const unsigned int& critid: critids) 
                                                        {
                                                            MaxTSMD(critid)=-(20 + 1.3 * clay - 0.01 * pow(clay,2)) * (site_arma(0)/23);
                                                            MaxTSMDb(critid)=-(20 + 1.3 * clay - 0.01 * pow(clay,2)) * (site_arma(0)/23); //1.0.1
                                                        }
                                                        Mvec = pvec - ETPvec;       
                                                        if (Mvec(0)>0) RateW(0)=0;
                                                        else RateW(0)=Mvec(0);
                                                        if (RateW(0)<=MaxTSMD(0)) RateW(0)=MaxTSMD(0); //ab 1.0.1
                                                        for(unsigned int iw=1; iw<tseqlength_unsi_m; iw++) 
                                                        {
                                                            if (RateW(iw-1)+Mvec(iw)<0) RateW(iw)=RateW(iw-1)+Mvec(iw);
                                                            else RateW(iw)=0;
                                                            if (RateW(iw)<=MaxTSMD(iw)) RateW(iw)=MaxTSMD(iw);
                                                        }
                                                        for(unsigned int iw=0; iw<tseqlength_unsi_m; iw++) 
                                                        {
                                                            if (RateW(iw)>=0.444*MaxTSMDb(iw)) RateWb(iw)=theta_arma(5);
                                                            else RateWb(iw)=(theta_arma(6) + (theta_arma(5)-theta_arma(6)) * ((MaxTSMDb(iw)-RateW(iw))/(MaxTSMDb(iw) - 0.444 * MaxTSMDb(iw))));
                                                        }
                                                        RateC.fill(1);
                                                        for(const unsigned int& critid: critids) RateC(critid)=0.6; //soil coverage factor  
                                                        
                                                        if (timediv==1) //annual mode: calculate xi first and then take mean of year
                                                        {
                                                            arma::colvec monthly_xi(tseqlength_unsi_m);
                                                            arma::colvec annual_xi(tseqlength_unsi);
                                                            monthly_xi = RateT % RateWb % RateC; 
                                                            for(unsigned int i_a=0; i_a<tseqlength_unsi; i_a++) annual_xi(i_a) = arma::mean(monthly_xi(arma::span(i_a*12,i_a*12+11)));
                                                            xi_arma.each_col() = annual_xi; 
                                                        }
                                                        if (timediv!=1) xi_arma.each_col() = RateT % RateWb % RateC;  
                                                    }   

                                                    if (model.get_cstring()==ctool || model.get_cstring()==ctool_o)
                                                    {   
                                                        double clay = site_arma(0); //if clay is known
                                                        double R = 1.67*(1.85+1.6*exp(-7.86*clay/100)); 
                                                        double h = 1/(R+1);              
                                                        double ai2_1 = (1-theta_arma(6))*h*theta_arma(0); //from FOMt to HUMt
                                                        double ai4_1 = theta_arma(6)*theta_arma(0); //from FOMt to FOMs
                                                        double ai3_2 = theta_arma(8)*theta_arma(1); //from HUMt to ROMt
                                                        double ai5_2 = (1-theta_arma(7)-theta_arma(8))*theta_arma(1); //from FOMt to FOMs
                                                        double ai6_3 = (1-theta_arma(7))*theta_arma(2); //from ROMt to ROMs
                                                        double ai4_4 = theta_arma(6)*theta_arma(3); //from FOMs to FOMs
                                                        double ai5_4 = (1-theta_arma(6))*h*theta_arma(3); //from FOMs to HUMs
                                                        double ai6_5 = theta_arma(8)*theta_arma(4); //from HUMs to ROMs
                                                        double ai5_5 = (1-theta_arma(7)-theta_arma(8))*theta_arma(4); //from HUMs to HUMs
                                                        double ai6_6 = (1-theta_arma(7))*theta_arma(5); //from ROMs to ROMs
   
                                                        arma::mat A1(6,6,arma::fill::zeros);
                                                        arma::rowvec ks ({theta_arma(0),theta_arma(1),theta_arma(2),theta_arma(3),theta_arma(4),theta_arma(5)});
                                                        A1.diag() = -ks;               
                                                        A1(1,0) = A1(1,0) + ai2_1;
                                                        A1(3,0) = A1(3,0) + ai4_1;
                                                        A1(2,1) = A1(2,1) + ai3_2;
                                                        A1(4,1) = A1(4,1) + ai5_2;
                                                        A1(5,2) = A1(5,2) + ai6_3;
                                                        A1(3,3) = A1(3,3) + ai4_4;
                                                        A1(4,3) = A1(4,3) + ai5_4;
                                                        A1(4,4) = A1(4,4) + ai5_5;
                                                        A1(5,4) = A1(5,4) + ai6_5;
                                                        A1(5,5) = A1(5,5) + ai6_6;
                                                        A_arma=A1;
                           
                                                        if (uncert==0)
                                                        {
                                                            int timediv_level0; //when annual mode also 12, because environmental input must be monthly
                                                            if (timediv==1) timediv_level0 = 12;
                                                            else timediv_level0 = timediv;
                                                        
                                                            arma::vec Tvec = env_in_arma.col(0); 
                                                            arma::vec doy (timediv_level0); 
                                                            arma::mat T_est (Tvec.n_elem,2);
                                                            int years = Tvec.n_elem/timediv_level0;
                                                            double offset = 110;
                                                            arma::vec ft_t (tsim);
                                                            arma::vec ft_s (tsim);
                 
                                                            doy = arma::regspace(0,timediv_level0-1)*365.25/timediv_level0+365.25/timediv_level0*0.5;   //monthly & annually: =({15.21875,45.65625,76.09375,106.53125,136.96875,167.40625,197.84375,228.28125,258.71875,289.15625,319.59375,350.03125});
  
                                                            for (int dpth=2; dpth<4; dpth++) //adäquat zu 2. und 3. Spalte von T.est in call.calculate_sote_from_monthly_data_areezo.R
                                                            {
                                                                for (int year=0; year<years;year++)
                                                                { 
                                                                    int firstelem = timediv_level0*year;
                                                                    int lastelem = timediv_level0*year+timediv_level0-1;                                                                   
                                                                    arma::vec subT = Tvec.subvec(firstelem,lastelem);
                                                                    double avgTemperature = mean(subT);

                                                                    arma::vec P = { 0.25, 0.75 };
                                                                    arma::vec Q = quantile(subT, P); //Armadillo nutzt Typ 5 von R: quantile(x, type=5). Standard ist aber type=7. Deshalb leichte Abweichungen von Rlibs.        
                                                                  
                                                                    double amplitude = Q(1)-Q(0);                                             
                                                                    double rho=1.992385e-07; //=3.1415926*2.0/(365.0*24.0*3600.0); // rho angular frequency; solid angle 
                                                                    //double Th_diff=0.35E-6; // Th_diff: thermal diffusivity of a solid, e.g. soil
                                                                    double dampingDepth=1.874401; //=pow((2*Th_diff/rho),0.5);
              
                                                                    arma::vec retVal = avgTemperature + amplitude * exp(-(dpth/10.0)/dampingDepth) * sin(rho*(doy-offset)*24.0*3600.0 - (dpth/10.0)/dampingDepth);
                                                                    T_est(arma::span(firstelem,lastelem),dpth-2) = retVal;
                                                                }
                                                            }
                                                            if (timediv==1) //annual mode
                                                            {
                                                                arma::mat T_est_annual (floor(Tvec.n_elem/12),2);
                                                                for(unsigned int i_a=0; i_a<tseqlength_unsi; i_a++) 
                                                                {
                                                                    ft_t(i_a) = arma::mean(7.24*exp(-3.432+0.168*T_est(arma::span(i_a*12,i_a*12+11),0)%(1-0.5*T_est(arma::span(i_a*12,i_a*12+11),0)/36.9)));
                                                                    ft_s(i_a) = arma::mean(7.24*exp(-3.432+0.168*T_est(arma::span(i_a*12,i_a*12+11),1)%(1-0.5*T_est(arma::span(i_a*12,i_a*12+11),1)/36.9)));   
                                                                }
                                                            }
                                                            else
                                                            {
                                                                ft_t = 7.24*exp(-3.432+0.168*T_est.col(0)%(1-0.5*T_est.col(0)/36.9));
                                                                ft_s = 7.24*exp(-3.432+0.168*T_est.col(1)%(1-0.5*T_est.col(1)/36.9));                                                             
                                                            }
                                                            
                                                            xi_arma.col(0) = ft_t;
                                                            xi_arma.col(1) = ft_t;
                                                            xi_arma.col(2) = ft_t;
                                                            xi_arma.col(3) = ft_s;
                                                            xi_arma.col(4) = ft_s;
                                                            xi_arma.col(5) = ft_s;
                                                        }
                                                    }
                                                }
                                                if (model_def && CTool_input_raw && (model.get_cstring()==ctool || model.get_cstring()==ctool_o))
                                                {
                                                    //calculate input for single pools when using raw input data
                                                    double R = 1.67*(1.85+1.6*exp(-7.86*site_arma(0)/100));
                                                    double fhum = std::max(0.0,theta_arma(9)-1/(R+1));
                                                
                                                    arma::mat Cin_raw = Cin_arma;
                                                    arma::mat Nin_raw = Nin_arma;
                                                    Cin_arma.set_size(tsim,nr_pools);
                                                    Nin_arma.set_size(tsim,nr_pools);                
                                                    Cin_arma.col(0)= Cin_raw.col(0)*(1-fhum);
                                                    Cin_arma.col(1)= Cin_raw.col(0)*fhum;
                                                    Cin_arma.col(2).fill(0);
                                                    Cin_arma.col(3)= Cin_raw.col(1)*(1-fhum);
                                                    Cin_arma.col(4)= Cin_raw.col(1)*fhum;
                                                    Cin_arma.col(5).fill(0);    
                                                    Nin_arma.col(0)= Nin_raw.col(0)*(1-fhum);
                                                    Nin_arma.col(1)= Nin_raw.col(0)*fhum;
                                                    Nin_arma.col(2).fill(0);
                                                    Nin_arma.col(3)= Nin_raw.col(1)*(1-fhum);
                                                    Nin_arma.col(4)= Nin_raw.col(1)*fhum;
                                                    Nin_arma.col(5).fill(0); 
                                                }  

                                                //////////////////////////////////////////////////////////////////////////////////////////////////////////
                                                //initial CN ratio estimation
                                                //////////////////////////////////////////////////////////////////////////////////////////////////////////         

                                                double CNinit=0.0;
                                                
                                                if (calcN0 && calcN)
                                                {
                                                    arma::vec meas_data_col_0(meas_data.n_rows);
                                                    meas_data_col_0=meas_data.col(0);
                                                    arma::vec meas_data_col_1(meas_data.n_rows);
                                                    meas_data_col_1=meas_data.col(1);
                                                    arma::vec meas_data_col_2(meas_data.n_rows);
                                                    meas_data_col_2=meas_data.col(2);
                                                    
                                                    arma::uvec ids1CN = find( (meas_data_col_2 > 0) && (meas_data_col_2 < 900) && (meas_data_col_1 > 0) && (meas_data_col_1 < 900) ); // Find indices with good C and N
                                                    arma::uvec ids1N = find( (meas_data_col_2 > 0) && (meas_data_col_2 < 900)  ); // Find indices with good N
                                                    
                                                    arma::uvec ids1;
                                                    if (calcC0) ids1=ids1CN;
                                                    else ids1=ids1N;
                                                        
                                                    
                                                    if (ids1.n_elem < 3)
                                                    {
                                                            String cn1="";
                                                            if (calcC0) cn1 ="too few (valid) N or C data points to calculate NO using calculated CN ratio"; 
                                                            else cn1 ="too few (valid) N data points to calculate NO using calculated CN ratio"; 
                                                            if (multisite)
                                                            {
                                                                String cn9999 = sitelist_;
                                                                cn1 +=" , Stand: ";
                                                                cn1 +=cn9999;
                                                            }    
                                                            throw exception(cn1.get_cstring());
                                                    }    

                                                    
                                                    if (ids1.n_elem>1) 
                                                    {
                                                        arma::vec C_C_N(ids1.n_elem); //C with C and N ids
                                                        arma::vec N_C_N(ids1.n_elem); //N with C and N ids
                                                        arma::mat t_C_N(ids1.n_elem,1); //time with C and N ids
                                                        if (calcC0) C_C_N = meas_data_col_1.elem(ids1); //C with C and N ids
                                                        else C_C_N = C0_arma;
                                                        N_C_N = meas_data_col_2.elem(ids1); //N with C and N ids
                                                        t_C_N = meas_data_col_0.elem(ids1); //time with C and N ids         
                                                        double mean_cn=0;
                                 
                                                        arma::uvec ids2; 
                                                        //outliers, we suppose that c:n is relatively constant - remove all n where cn is 20% above or below average for initial cn calculation
                                                        if (calcC0) 
                                                        {
                                                            mean_cn=mean(C_C_N/N_C_N); 
                                                            if (ids1.n_elem>3) ids2 = find( abs(100.0/mean_cn* C_C_N / N_C_N-100.0)<20.0 ); // Find indices N without outliers
                                                            else ids2 = ids1;
                                                        }
                                                        else
                                                        {
                                                            ids2 = ids1;                
                                                        }
                                            
                                                        if (meas_data_col_2(0)>0) // #if first meas point has N 
                                                        {
                                                            arma::mat t_C_N_int(ids2.n_elem,2); //include intercept for fastLm function
                                                            arma::mat CN_C_N(ids2.n_elem,1); //CN with C and N ids;
                                                            t_C_N = t_C_N-t_C_N(0); //to get value at first measurement point (=set to 0 at pos 0) (also if outlier)     
                                                            t_C_N = t_C_N(ids2); //exclude outliers
                                                            t_C_N_int.col(0).fill(1); //for intercept ... we need to prepare the data differently, by adding the column of 1s to the matrix (this is the intercept)...not included in fastLm function 
                                                            t_C_N_int.col(1) = t_C_N;        
                                                            if (calcC0)
                                                            {
                                                                //determine CN with CN regression
                                                                CN_C_N=C_C_N(ids2)/N_C_N(ids2); 
                                                                List lr1 = fastLm(CN_C_N, t_C_N_int); 
                                                                arma::colvec lr1_coef = lr1["coefficients"];
                                                                CNinit=std::max(0.0,lr1_coef(0));
                                                            }
                                                            else
                                                            {
                                                                //determine CN with N regression
                                                                CN_C_N=N_C_N(ids2); 
                                                                List lr1 = fastLm(CN_C_N, t_C_N_int); 
                                                                arma::colvec lr1_coef = lr1["coefficients"];
                                                                CNinit=sum(C0_arma)/std::max(0.0,lr1_coef(0));
                                                            }
                                                            
                                                        }
                                                        else
                                                        {
                                                            CNinit=C_C_N(0)/N_C_N(0); //determine CN without regression at first meas point if first meas point has no N
                                                        }
                                                    }
                                                }
                                                else
                                                {
                                                    if(C0.isNotNull() && N0.isNotNull()) CNinit=sum(C0_arma)/sum(N0_arma);
                                                }
                           
                                                //////////////////////////////////////////////////////////////////////////////////////////////////////////
                                                //C0
                                                ////////////////////////////////////////////////////////////////////////////////////////////////////////// 
                                          
                                                // if C0 is scalar, use fracts
                                                if (!calcC0 && C0_arma.n_elem == 1 && nr_p_unsi!=1) 
                                                {   
                                                    if ((model.get_cstring()!=rothc) || C0_fracts.isNotNull())  C0_arma=C0_fracts_arma*C0_arma(0) ;
                                                    else
                                                    {
                                                        double CN_sp = 0.0; //just for initialization. arrived here, 1 of the 2 following should make CN_sp > 0;
                                                        if (site_arma(3)>0) CN_sp = site_arma(3);
                                                        if (site_arma(3)==0) CN_sp = CNinit;
                                                        
                                                        arma::rowvec fractI = {0.59, 0.41, 0};
                                                        arma::rowvec C_in_tot(nr_pools);
                                                        C_in_tot=arma::sum(Cin_arma,0);
                                                           
                                                        if (RothC_dpmrpm_arma.n_elem==1) dpmrpm=RothC_dpmrpm_arma(0);
                                                        else dpmrpm=RothC_dpmrpm_arma(el_lists);
                                                        if (RothC_Cin4C0==TRUE && C_in_tot(1)>0 && C_in_tot(0)>0)
                                                        {
                                                            double fract1 = C_in_tot(0)/(C_in_tot(0)+C_in_tot(1));
                                                            fractI = {fract1, 1-fract1,0} ;
                                                        }
                                                        if (RothC_Cin4C0==FALSE) //use RothC_dpmrpm 
                                                        {   
                                                            fractI = {1/(1+1/dpmrpm), 1/(1+dpmrpm),0};
                                                        }   
                                                        C0_arma=C0_analyt(CN_sp, mean(xi_arma.col(0)), theta_arma(0), theta_arma(1), theta_arma(2), theta_arma(3), site_arma(1), site_arma(2), C0_arma(0),fractI ) ; 
                                                    }
                                                }
                                        
                                                if (calcC0)
                                                {   
                                                    arma::vec meas_data_col_0(meas_data.n_rows);
                                                    meas_data_col_0=meas_data.col(0);
                                                    arma::vec meas_data_col_1(meas_data.n_rows);
                                                    meas_data_col_1=meas_data.col(1);
                                                    
                                                    arma::uvec ids1 = find( (meas_data_col_1 > 0) && (meas_data_col_1 < 900) ); // Find indices with good C
                                                    if (ids1.n_elem < 3) 
                                                    {
                                                            String cn1 ="too few (valid) C data points to calculate CO"; 
                                                            if (multisite)
                                                            {
                                                                String cn9999 = sitelist_;
                                                                cn1 +=" , Stand: ";
                                                                cn1 +=cn9999;
                                                            }    
                                                            throw exception(cn1.get_cstring());
                                                    }   
                                                        
                                                    arma::vec C_C(ids1.n_elem); //C with C and N ids
                                                    arma::mat t_C(ids1.n_elem,2); //time with C and N ids, first column is for intercept
                                                    C_C = meas_data_col_1.elem(ids1); //C with C and N ids
                                                    t_C.col(1) = meas_data_col_0.elem(ids1); //time with C and N ids   
                                                    t_C.col(0).fill(1); //for intercept ... we need to prepare the data differently, by adding the column of 1s to the matrix (this is the intercept)...not included in fastLm function 
                                            
                                                    List lr2= fastLm(C_C, t_C); 
                                                    arma::colvec lr2coef = lr2["coefficients"];
                                                    
                                                    //RothC special
                                                    if ((model.get_cstring()==rothc) && C0_fracts.isNull()) //RothC Rene Standard 
                                                    {
                                                        if (RothC_dpmrpm_arma.n_elem==1) dpmrpm=RothC_dpmrpm_arma(0);
                                                        else dpmrpm=RothC_dpmrpm_arma(el_lists);
                                                        double CN_sp = 0.0; //just for initialization. arrived here, 1 of the 2 following should make CN_sp > 0
                                                        if (site_arma(3)>0) CN_sp = site_arma(3);
                                                        if (site_arma(3)==0) CN_sp = CNinit;
                                                        
                                                        arma::rowvec fractI = {0.59, 0.41, 0};
                                                        arma::rowvec C_in_tot(nr_pools);
                                                        C_in_tot=arma::sum(Cin_arma,0);
                                                        if (RothC_Cin4C0==TRUE && C_in_tot(1)>0 && C_in_tot(0)>0)
                                                        {
                                                            double fract1 = C_in_tot(0)/(C_in_tot(0)+C_in_tot(1));
                                                            fractI = {fract1, 1-fract1,0} ;
                                                        }
                                                        if (RothC_Cin4C0==FALSE) //use RothC_dpmrpm 
                                                        {   
                                                            fractI = {1/(1+1/dpmrpm), 1/(1+dpmrpm),0};
                                                        }   
                                                        C0_arma=C0_analyt(CN_sp, mean(xi_arma.col(0)), theta_arma(0), theta_arma(1), theta_arma(2), theta_arma(3), site_arma(1), site_arma(2), lr2coef(0),fractI ) ;            
                                                    }
                                                    else C0_arma=lr2coef(0)*C0_fracts_arma; 
                                                    if (init_info == TRUE)
                                                    {    
                                                        arma::rowvec C0_r = arma::conv_to<arma::rowvec>::from(C0_arma);
                                                        Rcout << "calculated a C0 of: " << C0_r;
                                                    }
                                                } 
             
                                                //////////////////////////////////////////////////////////////////////////////////////////////////////////
                                                //N0 calculation
                                                ////////////////////////////////////////////////////////////////////////////////////////////////////////// 
                           
                                                arma::mat Cin_arma_wood(tsim,nr_pools); //define beforehand, later used again
                                                if (wood) Cin_arma_wood=sum(Cin_wood_arma,2) ;
                                                if (calcN0 && calcN)  
                                                {
                                                    //arma::mat Cin_arma_wood(tsim,nr_pools);
                                                    arma::mat Nin_arma_wood(tsim,nr_pools);                       
                                                    if (wood) {Nin_arma_wood=Nin_prev_wood ;}
                                                    else
                                                    {
                                                        Cin_arma_wood = Cin_arma;
                                                        Nin_arma_wood = Nin_arma;
                                                    }
                                                        
                                                    arma::mat CN_in(tsim,nr_pools);
                                                    CN_in=Cin_arma_wood/Nin_arma_wood;
                                                    
                                                    // replace each NaN with 0
                                                    CN_in.replace(arma::datum::nan, 0);  
                                                    
                                                    arma::rowvec C_in_tot(nr_pools);
                                                    C_in_tot=arma::sum(Cin_arma_wood,0);
                                                    
                                                    arma::colvec weight_c(tsim);
                                                    arma::colvec weight_t(tsim);
                                                    arma::colvec weight_total(tsim);
                                                    
                                                    //temporal weight
                                                    weight_t = 1.0 / pow( arma::linspace(1, tsim,tsim) ,2.0);
                                                    bool sizechanged = FALSE;
                                                    
                                                    arma::uvec fpt = find((pt_arma==1) || (pt_arma==4)); // Find indices of fast pools
                                            
                                                    if (init_info && multisite)
                                                    {
                                                        Rcout << "\nXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX \n"; 
                                                        Rcout << "stand: " << sitelist_ << " \n"; 
                                                    }
   
                                                    for(const unsigned int& pt: fpt) 
                                                    {
                                                        double cn_w;
                                                        if (calcCN_fast_init)
                                                        {
                                                            //too few input to explain start conditions? then add start condition with presumed CN of 40, with difference in C0
                                                            if(C_in_tot(pt)<(C0_arma(pt))) 
                                                            {   
                                                                if (init_info == TRUE)
                                                                {
                                                                    Rcout << "\ntoo few C input to explain start conditions for pool: " << pt << " \n";
                                                                    Rcout << "total C input: " << C_in_tot(pt) << " \n";
                                                                    Rcout << "C0: " << C0_arma(pt) << " \n";
                                                                    Rcout << "No problem. For the calculation of initial N we just fill the missing C using standard CN of " << CNfast << ". \n";
                                                                }
                                                                weight_t.set_size(tsim+1);
                                                                weight_t = 1.0 / pow((arma::linspace(0, tsim,tsim+1) + 1.0),2.0);
                                                                
                                                                sizechanged = TRUE;
                                                                
                                                                arma::colvec C_in_mod(tsim); 
                                                                C_in_mod = Cin_arma_wood.col(pt);
                                                                C_in_mod.resize(tsim+1);
                                                                C_in_mod = arma::shift(C_in_mod,+1);
                                                                C_in_mod(0)=C0_arma(pt)-C_in_tot(pt);
                                                                
                                                                arma::colvec CN_in_mod(tsim);
                                                                CN_in_mod = CN_in.col(pt);
                                                                CN_in_mod.resize(tsim+1);
                                                                CN_in_mod = arma::shift(CN_in_mod,+1);
                                                                CN_in_mod(0) = CNfast;
                                                                
                                                                weight_c = C_in_mod/C_in_tot(pt); //mass weight
                                                                weight_total = (weight_t%weight_c)/sum(weight_t%weight_c);
                                                                cn_w = w_mean( CN_in_mod,weight_total);
                                                            }
                                                            else 
                                                            {
                                                                weight_c = Cin_arma_wood.col(pt) /C_in_tot(pt); //mass weight
                                                                weight_total=(weight_t%weight_c)/sum(weight_t%weight_c);
                                                                cn_w = w_mean(CN_in.col(pt),weight_total);
                                                            }
                                                        }
                                                        else cn_w = CNfast;
                                                        
                                                        N0_arma(pt)=C0_arma(pt)/cn_w;
                                                        arma::colvec yo = CN_in.col(pt); 
                                                        if ( (init_info == TRUE) && (any(yo(arma::find(yo > 0)) <5)) )  Rcout << "Warning! At least one input has CN ratio of < 5. Is that how it is meant to be? Pool id:" << pt << " \n";
                                                        if (sizechanged) weight_t.resize(tsim); //resize to normal for next pool
                                                    }//fast pools
                                        
                                                    bool N0ok=FALSE;
                                                    int cn_counter=0;
                                                    while (!N0ok)
                                                    {
                                                        cn_counter+=1;
                                                        arma::vec CN0=C0_arma;   
                                                        double N0_fast_top =0; 
                                                        double N0_fast_sub =0; 
                                                        double C0_top =0;
                                                        double C0_sub =0;
                                                        
                                                        N0_arma(find((pt_arma==2) || (pt_arma==5)))=C0_arma(find((pt_arma==2) || (pt_arma==5)))/CNbio; //bio/hum (pooltypes 2,5)
                                                        N0_fast_top=sum(N0_arma(find(pt_arma<3))); //bio/hum in topsoil (pooltypes 1,2)
                                                        N0_fast_sub=sum(N0_arma(find((pt_arma>3) && (pt_arma<6)))); //bio/hum in subsoil (pooltypes 4,5)  
                                                        C0_top=sum(C0_arma(find(pt_arma<4))); //topsoil (pooltypes 1,2,3)
                                                        C0_sub=sum(C0_arma(find(pt_arma>3))); //subsoil (pooltypes 4,5,6)
                                                    
                                                        N0_arma.elem(find(pt_arma==3)).fill( (C0_top - CNinit * N0_fast_top) / CNinit ); // adjust N0 of resistant pool top
                                                        N0_arma.elem(find(pt_arma==6)).fill( (C0_sub - CNinit * N0_fast_sub) / CNinit ); // adjust N0 of resistant pool sub
                                                        
                                                        CN0=C0_arma/N0_arma; 
                                                        
                                                        if ( (all(N0_arma.elem(find(pt_arma==3) )==0)) || (all(CN0.elem(find(pt_arma==3) )==0))   )
                                                        {
                                                            String cn1 ="Calculation of N0 not possible. C0 or N0 have zeros in all resistant pools."; 
                                                            if (multisite)
                                                            {
                                                                String cn9999 = sitelist_;
                                                                cn1 +=" , Stand: ";
                                                                cn1 +=cn9999;
                                                            }    
                                                            throw exception(cn1.get_cstring());
                                                        }

                                                        if ( (all(N0_arma.elem(find(pt_arma==3) )>=0)) &&  (all(CN0.elem(find(pt_arma==3) )>=CNbio)) && (all(CN0.elem(find(pt_arma==3) )<=100))   ) N0ok=TRUE;
                                                        if (!N0ok) 
                                                        {   
                                                            if ( any(CN0.elem(find(pt_arma==3) )>100) || any(N0_arma.elem(find(pt_arma==3) )<0)) CNbio+=0.1; //raise by 0.1
                                                            else if ( any(CN0.elem(find(pt_arma==3) )<CNbio) ) CNbio-=0.1; //lower by 0.1
                                                        }
                                                        if ( (CNbio>1000) || (cn_counter>1000) )
                                                        {
                                                            String cn1 ="Calculation of N0 not possible given the defined pool characteristics and input values."; 
                                                            if (cn_counter>1000) { cn1 +=" Perhaps initial CN ratio calculation lead to unrealistic value: "; cn1+=CNinit;}
                                                            if (multisite)
                                                            {
                                                                String cn9999 = sitelist_;
                                                                cn1 +=" , Stand: ";
                                                                cn1 +=cn9999;
                                                            }    
                                                            throw exception(cn1.get_cstring());
                                                        }                    
                                                    } 
                                                    
                                                    //1.2.1 deleted section 'topsoil check for N0 at C0': "if (all(C0_arma.elem(find(pt_arma==3) )==0) )//case there is no C in res/inert pools..." 
                                                    // is already excluded by "if ( (all(N0_arma.elem(find(pt_arma==3) )==0)) || (all(CN0.elem(find(pt_arma==3) )==0))   )..." see above

                                                    //subsoil check for N0 at C0 
                                                    if (all(C0_arma.elem(find(pt_arma==6) )==0) )//case there is no C in res/inert pool
                                                    {
                                                                //transfer all N to first slow pool
                                                            N0_arma.elem(find(pt_arma==5,1))+=sum(N0_arma.elem(find(pt_arma==6)));
                                                            N0_arma.elem(find(pt_arma==6)).fill(0);
                                                        if (all(C0_arma.elem(find(pt_arma==5) )==0) )//case additionally there is no C in bio/hum pools
                                                        {       //transfer all N to first fast pool
                                                            N0_arma.elem(find(pt_arma==4,1))+=sum(N0_arma.elem(find(pt_arma==5))); //"..,1))"->only first element
                                                            N0_arma.elem(find(pt_arma==5)).fill(0);                    
                                                        }
                                                    }
                                            
                                                    if (init_info == TRUE)
                                                    {
                                                        if ((CNbio!=CN_bio) && any(C0_arma.elem(find(pt_arma==2) )>0)) Rcout << "\nIn order to explain N0 CN ratio of slow pools had to be set to " << CNbio << " \n";
                                                        
                                                        arma::rowvec CNcalc= arma::conv_to<arma::rowvec>::from( C0_arma / N0_arma) ;
                                                        arma::rowvec N0_r = arma::conv_to<arma::rowvec>::from(N0_arma);
                                                        arma::rowvec C0_r = arma::conv_to<arma::rowvec>::from(C0_arma);
                                                        Rcout << "calculated an N0 of: " << N0_r ;
                                                        Rcout << "and CN values of   : " << CNcalc ;
                                                        Rcout << "and initial total CN of: " << CNinit  <<"  " << sum(C0_arma)/sum(N0_arma) <<" \n"; 
                                                        
                                                        //changed for 1.2.1
                                                        arma::rowvec CNres_z = C0_arma.elem(find(pt_arma==3,1)) ;
                                                        arma::rowvec CNres_n = N0_arma.elem(find(pt_arma==3,1)) ;
                                                        arma::rowvec CNres = CNres_z / CNres_n;

                                                        if (CNres(0) < 3  ) 
                                                        {
                                                            Rcout << "CN in resistant/inert pool is very low: " << CNres(0) <<" \n"; 
                                                            Rcout << "This is most probably due to much more C in slow pools than in resistant/inert pools \n" ;
                                                        }
                                                    }
                                                }
                                                else //if N0 not calculated
                                                {
                                                    N0_arma = as<arma::vec>(N0_);
                                                    if (multisite) N0_arma = as<arma::vec>(N0_sl_);
                                                }
            
                                                //////////////////////////////////////////////////////////////////////////////////////////////////////////
                                                // SOC/SON model core
                                                //////////////////////////////////////////////////////////////////////////////////////////////////////////

                                                NumericMatrix cpools(tseqlength,nr_pools); 
                                                arma::mat cpools_arma = as<arma::mat>(cpools);
                                                
                                                NumericMatrix npools(tseqlength,nr_pools); 
                                                arma::mat npools_arma = as<arma::mat>(npools);
                                                
                                                arma::mat cpools_sub = arma::zeros(1,nr_pools); 
                                                arma::mat npools_sub = arma::zeros(1,nr_pools); 
                                                
                                                arma::mat K1_sub(1,nr_pools); 
                                                arma::mat K2_sub(1,nr_pools); 
                                                arma::mat K3_sub(1,nr_pools); 
                                                arma::mat cn_sub(1,nr_pools);  

                                                // N mineralization variables
                                                NumericMatrix npools_diff(tseqlength,3); 
                                                arma::mat npools_diff_arma = as<arma::mat>(npools_diff);
                                                NumericMatrix nmin(tseqlength,nr_pools); 
                                                arma::mat nmin_arma = as<arma::mat>(nmin);
                                                NumericMatrix nmin2(tseqlength,nr_pools); 
                                                arma::mat nmin2_arma = as<arma::mat>(nmin2);
                                                List nmin_i_list(nmin); 
                                                arma::cube nmin_i_cube = arma::zeros(tseqlength,nr_pools,nr_pools);

                                                // start values for C and N        
                                                cpools_arma.row(0)=arma::trans(C0_arma);
                                                npools_arma.row(0)=arma::trans(N0_arma);
                                                
                                                int ts_in=0;
                                                int ts_xi=0;                                   
                                                int set_seed_seed = floor(arma::randu()*100); //different seeds for different sites/repetitions
                                                set_seed(set_seed_seed);        
                                                for(int ts=1; ts<tseqlength; ts++) 
                                                {  
                                                                    if (!spinup) 
                                                                    {
                                                                        ts_xi=ts;
                                                                        ts_in=ts; 
                                                                    }
                                                                    if (spinup) 
                                                                    {
                                                                        if ((ts-1)-floor((ts-1)/timediv)*timediv==0) //corresponds to modulo, always start with january when december is over when timediv==12 (month modus)
                                                                        {
                                                                            int tsim_in=Cin_arma.n_rows;
                                                                            ts_in=floor(arma::randu()*(tsim_in)/timediv)*timediv+1; //multiple of 12 when timediv==12
                                                                            ts_xi=ts_in-1;
                                                                        }
                                                                        else 
                                                                        {
                                                                            ts_xi++;
                                                                            ts_in++;
                                                                        }
                                                  //Rcout << "ts" <<  ts << "_" << ts_in << " \n"; 
                                                  //readline("");
                                                                    }
                                                                                       
                                                                    arma::rowvec xi_new_0 = xi_arma.row(std::max(0,ts_xi-1));
                                                                    arma::rowvec xi_new_1 = xi_arma.row(ts_xi);
                                                                    arma::rowvec K1(nr_pools);
                                                                    arma::rowvec K2(nr_pools);
                                                                    arma::rowvec K3(nr_pools);
                                                                    arma::rowvec K4(nr_pools);
                                                                    arma::rowvec K1n(nr_pools);
                                                                    arma::rowvec K2n(nr_pools);
                                                                    arma::rowvec K3n(nr_pools);
                                                                    arma::rowvec K4n(nr_pools);
                                                                    arma::rowvec Cin_here(nr_pools);
                                                                    arma::rowvec Nin_here(nr_pools);

                                                                    int substeps=1;   
                                                                    arma::rowvec Nin_here_sum(nr_pools);
                                                                    Nin_here_sum.fill(0);

                                                                    for (int wood_slice=0; wood_slice < wood_length; wood_slice++)
                                                                    {
                                                                         
                                                                        double wood_part=1; 
                                                                        double wood_part_ss=1; //for some substep calculations would part will be 1, see below 
                                                                    
                                                                        if (wood) 
                                                                        {
                                                                            Cin_here=Cin_wood_arma.slice(wood_slice).row(std::max(0,ts_in-1));
                                                                            if (calcN) Nin_here=Nin_wood_arma.slice(wood_slice).row(std::max(0,ts_in-1));
                                                                            if (yasso15up ) 
                                                                            {      
                                                                                if (ts==1 && uncert==0) for (int poolnr=0; poolnr<nr_pools-1;poolnr++) wood_xi_slice(wood_slice, poolnr)= std::min(1.0,pow( 1+theta_arma(27)*wood_diam_arma(wood_slice)+theta_arma(28)*pow(wood_diam_arma(wood_slice),2.0),-std::abs(theta_arma(29)) ));
                                                                                wood_xi=wood_xi_slice.row(wood_slice);
                                                                            }
                                                                            if (sum(Cin_arma_wood.row(std::max(0,ts_in-1))) > 0) wood_part=sum(Cin_here)/sum(Cin_arma_wood.row(std::max(0,ts_in-1)));
                                                                            else 
                                                                            {  
                                                                                if (wood_slice==0) wood_part=1; //no input? then factor 1 for first slice
                                                                                else wood_part=0;
                                                                            }
                                                                            wood_part_ss=wood_part;
                                                                        }
                                                                        else
                                                                        {  
                                                                            Cin_here=Cin_arma.row(std::max(0,ts_in-1));
                                                                            if (calcN) Nin_here=Nin_arma.row(std::max(0,ts_in-1));
                                                                        }

                                                                        //////////////////////////////////////////////////////////////////////////////////////////////////////////
                                                                        //C-Pools calculation
                                                                        //////////////////////////////////////////////////////////////////////////////////////////////////////////
                         
                                                                        bool negatives=TRUE;

                                                                        while (negatives)
                                                                        {
                                                                            arma::mat add_pools_sub= arma::zeros(substeps,nr_pools); 
                                                                            cpools_sub=join_vert(cpools_arma.row(ts-1),add_pools_sub);
                                                                            if (calcN) npools_sub=join_vert(npools_arma.row(ts-1),add_pools_sub);
                                                                            if (calcN) cn_sub=npools_sub;
                                                                            bool anynegatives=false;
                                                                            arma::rowvec xi_new_0_ss = xi_new_0;
                                                                            
                                                                                for(int ss=1; ss<substeps+1; ss++)
                                                                                {
                                                                                    if (ss>1) xi_new_0_ss=xi_new_1;
                                                                                    if (ss==1) wood_part_ss = wood_part;
                                                                                    else wood_part_ss = 1; //because at first substep allready multiplied with wood_part; 
                                                                                    
                                                                                    //Runge Kutta 4th order, produces K1-K4 vectors, which elements correspond to the pools, their mean is calculated below 
                                                                                    K1=Cin_here/substeps + arma::trans((A_arma * arma::diagmat((wood_xi%xi_new_0_ss)/timediv/substeps)  * arma::trans(cpools_sub.row(ss-1)*wood_part_ss))) ;   
                                                                                    K2=Cin_here/substeps + arma::trans((A_arma * arma::diagmat((wood_xi%(xi_new_0_ss+xi_new_1))/2/timediv/substeps)  * arma::trans(cpools_sub.row(ss-1)*wood_part_ss+0.5*K1))) ; 
                                                                                    K3=Cin_here/substeps + arma::trans((A_arma * arma::diagmat((wood_xi%(xi_new_0_ss+xi_new_1))/2/timediv/substeps)  * arma::trans(cpools_sub.row(ss-1)*wood_part_ss+0.5*K2))) ;  
                                                                                    K4=Cin_here/substeps + arma::trans((A_arma * arma::diagmat((wood_xi%xi_new_1)/timediv/substeps)  * arma::trans(cpools_sub.row(ss-1)*wood_part_ss+K3))) ;

                                                                                    if (ss==1) 
                                                                                    {
                                                                                        K1_sub=K1;
                                                                                        K2_sub=K2;
                                                                                        K3_sub=K3;
                                                                                    }
                                                                                    
                                                                                    if (ss>1)
                                                                                    {
                                                                                        K1_sub=join_vert(K1_sub, K1);
                                                                                        K2_sub=join_vert(K2_sub, K2);
                                                                                        K3_sub=join_vert(K3_sub, K3);
                                                                                    }
                                                                                                                                 
                                                                                    //mean of Runge-Kutta-Vectors, produces new C for each pool
                                                                                    cpools_sub.row(ss)=(K1+ 2*K2 + 2*K3 +K4)/6+cpools_sub.row(ss-1)*wood_part;
                                                                                    if (ss>1)
                                                                                    {
                                                                                        cpools_sub.row(ss)=(K1+ 2*K2 + 2*K3 +K4)/6+cpools_sub.row(ss-1);
                                                                                    }                                                                
                                               
                                                                                    if ( any(cpools_sub.row(ss)<0)) //can happen with annual time steps, then Runge-Kutta 4 does not work, and the time steps must be reduced (here divided by 2)
                                                                                    {
                                                                                        anynegatives=TRUE;
                                                                                        break;
                                                                                    }
   
                                                                                    if (calcN) 
                                                                                    {
                                                                                        //////////////////////////////////////////////////////////////////////////////////////////////////////////
                                                                                        // N calculation  
                                                                                        //////////////////////////////////////////////////////////////////////////////////////////////////////////
                                                                                        
                                                                                        // deltaC = c.pools[ts-1,i] + c.In.here - c.pools[ts,i] = change in each c pool
                                                                                        // deltaN = n.pools[ts-1,i] + n.In.here - n.pools[ts,i] = change in each n pool
                                                                                        // deltaC/c.pools[ts,i] = deltaN/n.pools[ts,i]
                                                                                        // consequently:   

                                                                                        npools_sub.row(ss) = (npools_sub.row(ss-1)*wood_part_ss+Nin_here/substeps) / ( (cpools_sub.row(ss-1)*wood_part_ss+Cin_here/substeps-cpools_sub.row(ss)) / cpools_sub.row(ss) + 1 );
                                                                                 
                                                                                        //c.In for pools exist where n.In == 0?
                                                                                        for(int i=0; i<nr_pools;i++)   
                                                                                        {   if (Cin_arma(std::max(0,ts_in-1),i)!=0){
                                                                                            if (Nin_arma(std::max(0,ts_in-1),i)==0) throw exception("C Input implies N input (when calcN==true)! There is C input without N input!") ;
                                                                                        }}
                                                                                        
                                                                                        //somewhere no C or no C input? (occurs if you start from 0 like with C-Tool_org, else rare case)
                                                                                        if (any(cpools_sub.row(ss)==0) || any(cpools_sub.row(ss-1)+Cin_arma.row(std::max(0,ts_in-1))==0)) 
                                                                                        {
                                                                                            // no C, or no C before and no Cinput
                                                                                            for(int i=0; i<nr_pools;i++) 
                                                                                            {if ((cpools_sub(ss,i)==0) || (cpools_sub(ss,i) + Cin_here(i) ==0 ) ) npools_sub(ss,i)=0;}

                                                                                            // there is C in pool (and so should N), but there have not been N or N input before (filled by transfer)
                                                                                            for(int i=0; i<nr_pools;i++) 
                                                                                            {if ((cpools_sub(ss,i)>0) && (npools_sub(ss-1,i)==0) && (Nin_here(i) ==0) )  
                                                                                                npools_sub(ss,i) = cpools_sub(ss,i)/ (cpools_sub(ss,0)/npools_sub(ss,0)) ;} //assume that first pools have C and N ... should work if first pool is always the first pool that gets input... worked so far
                                                                                        }

                                                                                        // if spinup starts with zero C0 and NO, CN would be defined via inputs, can be changed with CN_spin
                                                                                        if (spinup && ((!multisite && !CN_spin.isNull()) || (multisite && (!CN_spin_sl.isNull()||!CN_spin.isNull()) ) ))  
                                                                                        {
                                                                                            for(int i=0; i<nr_pools;i++) 
                                                                                            {
                                                                                                if (npools_sub(ss-1,i)==0) 
                                                                                                {
                                                                                                    npools_sub(ss,i)=cpools_sub(ss,i)/CN_spin_arma(i);
                                                                                                    cn_sub(ss,i) = CN_spin_arma(i);
                                                                                                }
                                                                                            }
                                                                                        }
                                                                                        //calculate cn    
                                                                                        for(int i=0; i<nr_pools;i++) 
                                                                                        {
                                                                                            if (npools_sub(ss,i)>0) cn_sub(ss,i) = cpools_sub(ss,i)/npools_sub(ss,i);
                                                                                            if (npools_sub(ss,i)==0) cn_sub(ss,i) =9999999;
                                                                                        }
                                                                                    }   
                                                                                }         
                                                                            if (anynegatives==TRUE) {
                                                                                substeps=substeps*2;
                                                                             if (substeps>1024) throw exception("Successive itteration time loop hang. Please try using a finer temporal resolution and report error to sorcering developers!") ;
                                                                            }
                                                                            if (anynegatives==FALSE) negatives=false;
                                                                        } //while negatives      
                                        
                                                                        cpools_arma.row(ts)+=cpools_sub.row(substeps);
                                                                        if (calcN) Nin_here_sum += Nin_here;
                                                                        if (calcN) npools_arma.row(ts)+=npools_sub.row(substeps);               
                            
                                                                        ////////////////////////////////////////////////////////////////////////////////////////////////////////
                                                                        // Nmin calculation - How much C is released by the single pools? Divide by target cn and get n released.  Runge Kutta 4th order
                                                                        ////////////////////////////////////////////////////////////////////////////////////////////////////////

                                                                        if (calcN)
                                                                        {

                                                                                arma::mat nmin_sub = arma::zeros(substeps,nr_pools); 
                                                                                arma::rowvec xi_new_0_ss = xi_new_0;
                                                                                arma::mat nmin_i;
                                                                                
                                                                                arma::mat K1n_mat = arma::zeros(nr_pools,nr_pools); 
                                                                                arma::mat K2n_mat = arma::zeros(nr_pools,nr_pools); 
                                                                                arma::mat K3n_mat = arma::zeros(nr_pools,nr_pools); 
                                                                                arma::mat K4n_mat = arma::zeros(nr_pools,nr_pools); 
                                                        
                                                                                for(int ss=1; ss<substeps+1; ss++)
                                                                                {
                                                                                    if (ss>1) xi_new_0_ss=xi_new_1;
                                                                                    
                                                                                    if (ss>1) wood_part=1;
                                                                                    // Runge Kutta 4th order, produces K1n_mat-K4n_mat matrices, which rows correspond to the N source pools and which columns correspond to N sink pools,
                                                                                    // and produces K1n-K4n vectors that are the colsums = how many N origins from which pool
                                                                                    K1n_mat= A_arma *    arma::diagmat((  cpools_sub.row(ss-1)*wood_part ) % ((wood_xi%xi_new_0_ss)/timediv/substeps));  
                                                                                    arma::inplace_trans(K1n_mat);
                                                                                    K1n_mat= K1n_mat * arma::diagmat(1/cn_sub.row(ss));
                                                                                    
                                                                                    K2n_mat= A_arma *    arma::diagmat((  cpools_sub.row(ss-1)*wood_part +0.5 * K1_sub.row(ss-1)) % ((wood_xi%(xi_new_0_ss+xi_new_1))/2/timediv/substeps));                                 
                                                                                    arma::inplace_trans(K2n_mat);
                                                                                    K2n_mat= K2n_mat * arma::diagmat(1/cn_sub.row(ss));

                                                                                    K3n_mat= A_arma *    arma::diagmat((  cpools_sub.row(ss-1)*wood_part +0.5 * K2_sub.row(ss-1)) % ((wood_xi%(xi_new_0_ss+xi_new_1))/2/timediv/substeps));                                 
                                                                                    arma::inplace_trans(K3n_mat);
                                                                                    K3n_mat= K3n_mat * arma::diagmat(1/cn_sub.row(ss));        
                                                                                    
                                                                                    K4n_mat= A_arma *    arma::diagmat((  cpools_sub.row(ss-1)*wood_part + K3_sub.row(ss-1) ) % ((wood_xi%xi_new_1)/timediv/substeps));                                 
                                                                                    arma::inplace_trans(K4n_mat);
                                                                                    K4n_mat= K4n_mat * arma::diagmat(1/cn_sub.row(ss));
                        
                                                                                    arma::mat nmin_ss = -(K1n_mat + 2*K2n_mat + 2*K3n_mat + K4n_mat)/6; //N min of single pools
                                                                                    arma::mat nmin_rowsum = arma::sum(nmin_ss,1);
                                                                                    arma::inplace_trans(nmin_rowsum);
                                                                                    nmin_sub.row(ss-1)=nmin_rowsum; //total N min
                                                                                                                                                                     
                                                                                    for(int i=0; i<nr_pools;i++) 
                                                                                    {
                                                                                        if (cn_sub(ss,i)==9999999) 
                                                                                        {
                                                                                            nmin_sub(ss-1,i)=0;
                                                                                            nmin_ss.row(i).fill(0);
                                                                                        }
                                                                                    }
                                                                                        
                                                                                    if(ss==1)nmin_i = nmin_ss;
                                                                                    if(ss>1)nmin_i += nmin_ss;                               
                                                                                    
                                                                                }
                                                                                arma::mat nminrow = arma::zeros(substeps,nr_pools);  
                                                                                nminrow = arma::sum(nmin_sub,0); // =rowSums
                                                                                nmin_arma.row(ts) += nminrow;      
                                                                                
                                                                                if (wood) nmin2_arma.row(ts)=npools_arma.row(ts-1)+ Nin_prev_wood.row(std::max(0,ts_in-1)) - npools_arma.row(ts) ;   
                                                                                else      nmin2_arma.row(ts)=npools_arma.row(ts-1)+ Nin_arma.row(std::max(0,ts_in-1)) - npools_arma.row(ts) ;
                                                                                for(int i=0; i<nr_pools; i++) nmin_i_cube(arma::span(ts),arma::span::all,arma::span(i)) += nmin_i.row(i);
                                                                                
                                                                        } //calcN
                                                                    } //wood   
                                       
                                                                    //////////////////////////////////////////////////////////////////////////////////////////////////////////
                                                                    // N balance calculation
                                                                    //////////////////////////////////////////////////////////////////////////////////////////////////////////
                                                            
                                                                    if (calcNbalance)
                                                                    {
                                                                        npools_diff_arma(ts,0)=arma::sum(npools_arma.row(ts-1))-arma::sum(npools_arma.row(ts));
                                                                        npools_diff_arma(ts,1)=arma::sum(Nin_here_sum)+npools_diff_arma(ts,0)-arma::sum(nmin_arma.row(ts));
                                                                        npools_diff_arma(ts,2)=arma::sum(Nin_here_sum)+npools_diff_arma(ts,0)-arma::sum(nmin2_arma.row(ts));

                                                                    }
                                                } //end of time loop
   
                                                //////////////////////////////////////////////////////////////////////////////////////////////////////////
                                                //output
                                                //////////////////////////////////////////////////////////////////////////////////////////////////////////
                                                
                                                //define list output names
                                                CharacterVector list_names = {"C"};

                                                CharacterVector pool_i_names;

                                                for(int i=0; i<nr_pools; i++) 
                                                { 
                                                    String pool_i_name("pool_");
                                                    pool_i_name+=i+1;
                                                    pool_i_names.push_back(pool_i_name);
                                                }
                                   
                                                cpools=wrap(cpools_arma);
                                                colnames(cpools)=pool_i_names;
                                        
                                                listout(0)=cpools;
                                      
                                                if (calcN)
                                                {
                                                    int listrunner = 4;
                                                    npools=wrap(npools_arma);
                                                    nmin=wrap(nmin_arma);
                                                    nmin2=wrap(nmin2_arma);
                                                    colnames(npools)=pool_i_names;
                                                    colnames(nmin)=pool_i_names;
                                                    colnames(nmin2)=pool_i_names;
                                                    listout(1)=npools;
                                                    listout(2)=nmin2;
                                                    listout(3)=nmin;
                                                    list_names.push_back("N");
                                                    list_names.push_back("Nloss");
                                                    list_names.push_back("Nmin");
                                                    for(int i=0; i<nr_pools; i++) 
                                                    {
                                                        NumericMatrix a = wrap(nmin_i_cube.slice(i));
                                                        colnames(a)=pool_i_names;
                                                        listout(listrunner)= a ;
                                                        listrunner+=1;
                                                        String nmin_i_name("Nmin.sink.");
                                                        nmin_i_name+=i+1;
                                                        list_names.push_back(nmin_i_name);
                                                    }
                                                
                                                    if (calcNbalance)   
                                                    {
                                                        npools_diff=wrap(npools_diff_arma);
                                                        CharacterVector npool_diff_names={"d_N","N_bal1","N_bal2"};
                                                        colnames(npools_diff)=(npool_diff_names);
                                                        listout(listrunner)=npools_diff;
                                                        list_names.push_back("Nbalance");
                                                    }
                                                }
                                                listout.names()=list_names;
                                                
                                                if (multisite || unc_length>1) 
                                                {
                                                    if (unc_length>1) any_unc=true;
                                                    if (!calcN) listout2 = List::create(_["C"]  = listout[0]);
                                                    if (calcN ) 
                                                    {
                                                        listout2 = List::create(_["C"]  = listout[0] ,_["N"]  = listout[1] ,_["Nloss"]  = listout[2] ,_["Nmin"]  = listout[3]);
                                                        for(int i=0; i<nr_pools; i++) 
                                                        {        
                                                            String nmin_i_name("Nmin.sink.");
                                                            nmin_i_name+=i+1;
                                                            listout2.push_back(listout[4+i],nmin_i_name);
                                                        }
                                                        if (calcNbalance) listout2.push_back(listout[4+nr_pools],"Nbalance");
                                                    }
                                                    if (unc_length>1 && !multisite) listout_list.push_back(listout2);
                                                    if (unc_length==1 && multisite) listout_list.push_back(listout2,sitelist_); 
                                                    if (unc_length>1 && multisite) listout_list_unc.push_back(listout2,sitelist_);
                                                }  
                        } //uncert end                   
                        if (multisite && unc_length>1 && donotwrite==FALSE ) listout_list.push_back(listout_list_unc,sitelist_);
    } //el_lists end
    if ((multisite || any_unc) && donotwrite==FALSE )
    {
       return listout_list;
    }
    else 
    {  
        return listout;
    }
}

