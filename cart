cd("F:/term1/DR/Julia")
using DataFrames;
using JuMP
using Gurobi
#########################################
ys=readtable("ys.csv"); 
hs=readtable("hs.csv");
C=readtable("C_minus.csv");
ny_o=size(ys)[1] #original number of youth
nh_o=size(hs)[1] #original number of houses
#######################
### make problem smaller
ny=100
nh=70
ys=ys[1:ny,:];
hs=hs[1:nh,:];

ny=size(ys)[1]
nh=size(hs)[1]

##############################################################################################################
#choosing features to work on
F_c=[:WHERE_SLEEP,:X17_OR_YOUNGER,:GET_ANY_MONEY_LEGAL_OR_OTHER,:DISABILITY,:TYPE_OF_EXIT,:TRIMORBIDITY,:MENTAL_HEALTH_ISSUE];
   # F_c=[:X17_OR_YOUNGER,:WHERE_SLEEP];
nf_c=size(F_c)[1];
#nk_f=zeros(Int8,nf_c);
nk_f=Dict();
#for f in 1:nf_c
for f in F_c
   nk_f[f]=size(levels(ys[f]))[1]
end
#max_nk_f=maximum(nk_f)
max_nk_f=maximum(values(nk_f))


function ind(x)
    if x==true
	1
    else
	0
    end
end

F_nc=[:nst];
    #F_nc=[];
nf_nc=size(F_nc)[1];
####################################################################################################################################
#tree structure
nn=6 # number of nodes
	nn=3
nl=7   #number of leaves
	nl=4
left=[[1 2 3],
    [4 5],
    [4],
    [6],        
    [1 2],
    [1]];
	left=[[1 2],
	    [1],
	    [3]];

right=[[4 5 6 7],
    [6 7],
    [5],
    [7],
    [3],
    [2]];
	right=[[3 4],
	    [2],
	    [4]];
####################################################################################
cart=Model(solver=GurobiSolver(MIPGap=1));
M=1000;
M2=100;
M3=100;
ep=0.001;
@variable(cart, r[1:ny,1:nh,1:nl]>=0);
@variable(cart, x[1:ny,1:nh,1:nl],Bin);
#@variable(cart, a_c[1:nn,1:nf_c],Bin);
@variable(cart, a_c[1:nn,F_c],Bin);
#@variable(cart, a_nc[1:nn,1:nf_nc],Bin);
@variable(cart, a_nc[1:nn,F_nc],Bin);
@variable(cart, b[1:nn]);
@variable(cart, gp[1:ny,1:nh,1:nn]>=0);
@variable(cart, gn[1:ny,1:nh,1:nn]>=0);
@variable(cart, w[1:ny,1:nh,1:nn],Bin);
#@variable(cart, sl[1:nn,1:nf_c,1:max_nk_f],Bin); #some of them has extra k
@variable(cart, sl[1:nn,F_c,1:max_nk_f],Bin); #some of them has extra k
#@variable(cart, sr[1:nn,1:nf_c,1:max_nk_f],Bin);
@variable(cart, sr[1:nn,F_c,1:max_nk_f],Bin);
@variable(cart, wl[1:ny,1:nh,1:nn],Bin);
@variable(cart, wr[1:ny,1:nh,1:nn],Bin);
@variable(cart, v[1:ny,1:nh,1:nl]);
@variable(cart, z[1:ny,1:nh,1:nl]);
st=Dict();
st[1]=[ 0.0457221; -0.185983  ;0.0  ;0.227834  ;0.0  ;0.0368177  ;0.0  ;0.156238];
st[2]=[ 0.0595911; -0.166154  ;0.0  ;0.131869  ;0.0  ;0.0206445  ;0.0  ;0.163078];
st[3]=[-0.752677; -0.638046];
@variable(cart, beta_y_RRH[i=1:8],start=st[1][i]); 
@variable(cart, beta_y_PSH[i=1:8],start=st[2][i]);
@variable(cart, beta_h[i=1:2],start=st[3][i]); # 1 RRH 2 PSH

@objective(cart,Min,sum(sum(sum(r[y,h,l] for y=1:ny) for h=1:nh)for l=1:nl) );

for n in 1:nn
    #@constraint(cart,sum(a_c[n,f] for f=1:nf_c)+sum(a_nc[n,f] for f=1:nf_nc)==1);
    @constraint(cart,sum(a_c[n,f] for f in F_c)+sum(a_nc[n,f] for f in F_nc)==1);
end
#######################################################################
####################### categoricals
for n in 1:nn
    #for f in 1:nf_c 
    for f in F_c 
        for k in 1:nk_f[f]
            @constraint(cart,sl[n,f,k]+sr[n,f,k]==a_c[n,f]);
        end
    end
end          
for y in 1:ny
    for h in 1:nh 
        for n in 1:nn
            if ys[y,:arrivedate]<=hs[h,:DATE_OF_EXIT_HOMELESSNESS]
                #for f in 1:nf_c
                for f in F_c
                    #@constraint(cart,wl[y,h,n]<=sum(sl[n,f,k]*d[y,h,f,k] for k=1:nk_f[f])+1-a_c[n,f]);
                    #@constraint(cart,wr[y,h,n]<=sum(sr[n,f,k]*d[y,h,f,k] for k=1:nk_f[f])+1-a_c[n,f]); 
                    @constraint(cart,wl[y,h,n]<=sum(sl[n,f,k]*ind(ys[y,f]==levels(ys[f])[k]) for k=1:nk_f[f] )+1-a_c[n,f]);
                    @constraint(cart,wr[y,h,n]<=sum(sr[n,f,k]*ind(ys[y,f]==levels(ys[f])[k]) for k=1:nk_f[f] )+1-a_c[n,f]); 
                end
                for l in right[n]
                    #@constraint(cart,x[y,h,l]<=wr[y,h,n]+1-sum(a_c[n,f] for f=1:nf_c)); 
                     @constraint(cart,x[y,h,l]<=wr[y,h,n]+1-sum(a_c[n,f] for f in F_c)); 
                end
                for l in left[n]
                    #@constraint(cart,x[y,h,l]<=wl[y,h,n]+1-sum(a_c[n,f] for f=1:nf_c)); 
                    @constraint(cart,x[y,h,l]<=wl[y,h,n]+1-sum(a_c[n,f] for f in F_c)); 
                end
            end
        end
    end
end
##########################################################
##### linearize
for y in 1:ny
    for h in 1:nh 
        if ys[y,:arrivedate]<=hs[h,:DATE_OF_EXIT_HOMELESSNESS]
            @constraint(cart,sum(x[y,h,l] for l=1:nl)==1);
            for l in 1:nl
                @constraint(cart,r[y,h,l]<=M2*x[y,h,l]);
                @constraint(cart,r[y,h,l]<=v[y,h,l]);
                @constraint(cart,r[y,h,l]>=v[y,h,l]-M2*(1-x[y,h,l]));
		@constraint(cart,v[y,h,l]>= C[(y-1)*(nh_o)+h,1]-z[y,h,l]);
                @constraint(cart,v[y,h,l]>= -C[(y-1)*(nh_o)+h,1]+z[y,h,l]);
            end
        end
    end
end
#put aside incompatibles
for y in 1:ny
    for h in 1:nh 
        for l in 1:nl
            if ys[y,:arrivedate]>hs[h,:DATE_OF_EXIT_HOMELESSNESS]
            @constraint(cart,r[y,h,l]==0);
            end
        end
    end
end
############################################################################################
####################### non categoricals
for y in 1:ny
    for h in 1:nh 
        for n in 1:nn
            if ys[y,:arrivedate]<=hs[h,:DATE_OF_EXIT_HOMELESSNESS]
            #@constraint(cart,b[n]-sum(a_nc[n,f]*dp[y,h,f] for f=1:nf_nc)==gp[y,h,n]-gn[y,h,n]);
            @constraint(cart,b[n]-sum(a_nc[n,f]*ys[y,f] for f in F_nc)==gp[y,h,n]-gn[y,h,n]);
            @constraint(cart,gp[y,h,n]<=M*w[y,h,n]); 
            @constraint(cart,gn[y,h,n]<=M*(1-w[y,h,n])); 
            @constraint(cart,gp[y,h,n]+gn[y,h,n]>=ep*(1-w[y,h,n])); 
            @constraint(cart,gp[y,h,n]+gn[y,h,n]>=ep*w[y,h,n]); 
            end
        end
    end
end
for y in 1:ny
    for h in 1:nh 
        for n in 1:nn
            if ys[y,:arrivedate]<=hs[h,:DATE_OF_EXIT_HOMELESSNESS]
                for l in right[n]
                    #@constraint(cart,x[y,h,l]<=1-w[y,h,n]+1-sum(a_nc[n,f] for f=1:nf_nc)); 
		    @constraint(cart,x[y,h,l]<=1-w[y,h,n]+1-sum(a_nc[n,f] for f in F_nc)); 
                end
                for l in left[n]
                    #@constraint(cart,x[y,h,l]<=w[y,h,n]+1-sum(a_nc[n,f] for f=1:nf_nc)); 
                    @constraint(cart,x[y,h,l]<=w[y,h,n]+1-sum(a_nc[n,f] for f in F_nc)); 
                end
            end
        end
    end
end
##################################
#piece wise linear combination with different policy for diffrent house type
for l in 1:nl
    for y in 1:ny
	for h in 1:nh
	    if ys[y,:arrivedate]<=hs[h,:DATE_OF_EXIT_HOMELESSNESS]	
		   @constraint(cart,z[y,h,l]>=hs[h,:TYPE_OF_EXIT_RRH]*(beta_h[1]+beta_y_RRH[1]*ys[y,:X17_OR_YOUNGER1]+beta_y_RRH[2]*ys[y,:WHERE_SLEEPcouch]+beta_y_RRH[3]*ys[y,:WHERE_SLEEPoutdoors]+beta_y_RRH[4]*ys[y,:WHERE_SLEEPshelter]+beta_y_RRH[5]*ys[y,:WHERE_SLEEPtransitional]+beta_y_RRH[6]*ys[y,:GET_ANY_MONEY_LEGAL_OR_OTHERy]+beta_y_RRH[7]*ys[y,:DISABILITYy]+beta_y_RRH[8]*ys[y,:nst])+hs[h,:TYPE_OF_EXIT_PSH]*(beta_h[2]+beta_y_PSH[1]*ys[y,:X17_OR_YOUNGER1]+beta_y_PSH[2]*ys[y,:WHERE_SLEEPcouch]+beta_y_PSH[3]*ys[y,:WHERE_SLEEPoutdoors]+beta_y_PSH[4]*ys[y,:WHERE_SLEEPshelter]+beta_y_PSH[5]*ys[y,:WHERE_SLEEPtransitional]+beta_y_PSH[6]*ys[y,:GET_ANY_MONEY_LEGAL_OR_OTHERy]+beta_y_PSH[7]*ys[y,:DISABILITYy]+beta_y_PSH[8]*ys[y,:nst])-M3*(1-x[y,h,l])); 

		   @constraint(cart,z[y,h,l]<=hs[h,:TYPE_OF_EXIT_RRH]*(beta_h[1]+beta_y_RRH[1]*ys[y,:X17_OR_YOUNGER1]+beta_y_RRH[2]*ys[y,:WHERE_SLEEPcouch]+beta_y_RRH[3]*ys[y,:WHERE_SLEEPoutdoors]+beta_y_RRH[4]*ys[y,:WHERE_SLEEPshelter]+beta_y_RRH[5]*ys[y,:WHERE_SLEEPtransitional]+beta_y_RRH[6]*ys[y,:GET_ANY_MONEY_LEGAL_OR_OTHERy]+beta_y_RRH[7]*ys[y,:DISABILITYy]+beta_y_RRH[8]*ys[y,:nst])+hs[h,:TYPE_OF_EXIT_PSH]*(beta_h[2]+beta_y_PSH[1]*ys[y,:X17_OR_YOUNGER1]+beta_y_PSH[2]*ys[y,:WHERE_SLEEPcouch]+beta_y_PSH[3]*ys[y,:WHERE_SLEEPoutdoors]+beta_y_PSH[4]*ys[y,:WHERE_SLEEPshelter]+beta_y_PSH[5]*ys[y,:WHERE_SLEEPtransitional]+beta_y_PSH[6]*ys[y,:GET_ANY_MONEY_LEGAL_OR_OTHERy]+beta_y_PSH[7]*ys[y,:DISABILITYy]+beta_y_PSH[8]*ys[y,:nst])+M3*(1-x[y,h,l]));
	    end
        end 
     end
end

status = solve(cart) 

#################
#results
x= getvalue(x);
a_nc= getvalue(a_nc)
b= getvalue(b)
a_c= getvalue(a_c)
sl= getvalue(sl)
sr= getvalue(sr)
for f in 1:nf_c
    println(levels(ys[F_c[f]]))
end

beta_y_RRH= getvalue(beta_y_RRH)
beta_y_PSH= getvalue(beta_y_PSH)
beta_h= getvalue(beta_h)


###############################################################
#####################
### constrainting for sth like warm start
warm=Dict()
warm[1] = @constraint(cart, zeros(size(beta_y_RRH)) .<= beta_y_RRH );
warm[2] = @constraint(cart, beta_y_RRH .<= ones(size(beta_y_RRH)) );
warm[3] = @constraint(cart, zeros(size(beta_y_PSH)) .<= beta_y_PSH );
warm[4] = @constraint(cart, beta_y_PSH .<= ones(size(beta_y_PSH)) );
warm[5] = @constraint(cart, zeros(size(beta_h)) .<= beta_h );
warm[6] = @constraint(cart, beta_h .<= ones(size(beta_h)) );
##########
#relaxation
JuMP.setRHS(warm[1][1], -1)
JuMP.setRHS(warm[2][1], 2)

##################################
##################################
##################################
@variable(cart, beta_y[1:8]);
@variable(cart, beta_h[1:2]);# 1 RRH 2 PSH
#piece wise linear combination
for l in 1:nl
    for y in 1:ny
	for h in 1:nh
	    if ys[y,:arrivedate]<=hs[h,:DATE_OF_EXIT_HOMELESSNESS]	
		   @constraint(cart,z[l]>=beta_y[1]*ys[y,:X17_OR_YOUNGER1]+beta_y[2]*ys[y,:WHERE_SLEEPcouch]+beta_y[3]*ys[y,:WHERE_SLEEPoutdoors]+beta_y[4]*ys[y,:WHERE_SLEEPshelter]+beta_y[5]*ys[y,:WHERE_SLEEPtransitional]+beta_y[6]*ys[y,:GET_ANY_MONEY_LEGAL_OR_OTHERy]+beta_y[7]*ys[y,:DISABILITYy]+beta_y[8]*ys	[y,:nst]+beta_h[1]*hs[h,:TYPE_OF_EXIT_RRH]+beta_h[2]*hs[h,:TYPE_OF_EXIT_PSH]-M3*(1-x[y,h,l])); 
		   @constraint(cart,z[l]<=beta_y[1]*ys[y,:X17_OR_YOUNGER1]+beta_y[2]*ys[y,:WHERE_SLEEPcouch]+beta_y[3]*ys[y,:WHERE_SLEEPoutdoors]+beta_y[4]*ys[y,:WHERE_SLEEPshelter]+beta_y[5]*ys[y,:WHERE_SLEEPtransitional]+beta_y[6]*ys[y,:GET_ANY_MONEY_LEGAL_OR_OTHERy]+beta_y[7]*ys[y,:DISABILITYy]+beta_y[8]*ys	[y,:nst]+beta_h[1]*hs[h,:TYPE_OF_EXIT_RRH]+beta_h[2]*hs[h,:TYPE_OF_EXIT_PSH]+M3*(1-x[y,h,l])); 
	    end
        end 
     end
end




####################################################
#fitt=glm(STILL_HOUSED ~ WHERE_SLEEP+X17_OR_YOUNGER+GET_ANY_MONEY_LEGAL_OR_OTHER+DISABILITY+TYPE_OF_EXIT , hptr,family=binomial(link = "logit"))
#fitt_n=glm(STILL_HOUSED ~ X17_OR_YOUNGER+TRIMORBIDITY+WHERE_SLEEP+nst+MENTAL_HEALTH_ISSUE+TYPE_OF_EXIT , nhptr,family=binomial(link = "logit"))

#encoding
d=zeros(ny,nh,nf_c,max_nk_f);
for f in 1:nf_c
    for k in 1:nk_f[f]
        for y in 1:ny
            for h in 1:nh
                if ys[y,:arrivedate]<=hs[h,:DATE_OF_EXIT_HOMELESSNESS]
                    if ys[y,F_c[f]]==levels(ys[F_c[f]])[k]
                        d[y,h,f,k]=1
                    end
                end
            end
        end
    end
end



dp=zeros(ny,nh,nf_nc);
for f in 1:nf_nc
    for y in 1:ny
        for h in 1:nh
            if ys[y,:arrivedate]<=hs[h,:DATE_OF_EXIT_HOMELESSNESS]
                dp[y,h,f]=ys[y,F_nc[f]]
            end
        end
    end
end
