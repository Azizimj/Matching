cd("F:/term1/DR/Julia")
using DataFrames;
using JuMP
using Gurobi
######################
ys=readtable("ys.csv"); 
hs=readtable("hs.csv");
p=readtable("p.csv");
p=p[:,2:end];
p_nh=readtable("p_nh.csv");
p_nh=p_nh[:,2:end]; 
p_go_nh=readtable("p_go_nh.csv");
p_go_nh=p_go_nh[:,2:end];
p=convert(Matrix,p);
p_nh=convert(Matrix,p_nh);
p_go_nh=convert(Matrix,p_go_nh);
df = Dates.DateFormat("y-m-d");
hs[:,:DATE_OF_EXIT_HOMELESSNESS]=Date(hs[:,:DATE_OF_EXIT_HOMELESSNESS],df); #arrive date of house
ys[:,:arrivedate]=Date(ys[:,:arrivedate],df); #arrivedate

#######################
### make problem smaller
ny=30
nh=30
ys=ys[1:ny,:];
hs=hs[1:nh,:];
p=p[1:ny,1:nh];
p_nh=p_nh[1:ny,:];
p_go_nh=p_go_nh[1:ny,:];

ny=size(ys)[1]
nh=size(hs)[1]

srand(1);
rho=rand(1,ny) #tie breaking rho;

############################################################
ph=Model(solver=GurobiSolver(TimeLimit=3600,NodefileStart=2,Threads=2));
M=500;
ep=0.01;

#@variable(ph, beta_y[1:8]);
#@variable(ph, beta_h);
st=Dict();
st[1]=[ 0.0457221; -0.185983  ;0.0  ;0.227834  ;0.0  ;0.0368177  ;0.0  ;0.156238];
st[2]=[ 0.0595911; -0.166154  ;0.0  ;0.131869  ;0.0  ;0.0206445  ;0.0  ;0.163078];
st[3]=[-0.752677; -0.638046];
@variable(ph, beta_y_RRH[i=1:8],start=st[1][i]); 
@variable(ph, beta_y_PSH[i=1:8],start=st[2][i]);
@variable(ph, beta_h[i=1:2],start=st[3][i]); # 1 RRH 2 PSH

@variable(ph, x[1:ny,1:nh]  ,Bin);
@variable(ph, z[1:ny,1:nh]  ,Bin);
@variable(ph, w[1:ny,1:ny,1:nh]  ,Bin);
@variable(ph, u[1:ny,1:ny,1:nh] ,Bin);
@variable(ph, vp[1:ny,1:ny,1:nh]>=0);
@variable(ph, vn[1:ny,1:ny,1:nh]>=0);
@variable(ph, 0.4 >=ei[1:3]>= -0.4);
@variable(ph, 1>=e>=0);


@objective(ph,Min,-( sum(sum(p[y,h]*x[y,h] for h=1:nh) for y=1:ny) + sum(p_go_nh[y]*p_nh[y] for y=1:ny)-sum(sum(p_go_nh[y]*p_nh[y]*x[y,h] for h=1:nh)for y=1:ny) )  );

for y in 1:ny
    @constraint(ph, sum(x[y,h] for h=1:nh) <= 1);
end
for h in 1:nh
    @constraint(ph, sum(x[y,h] for y=1:ny) <= 1);
end     
for y in 1:ny
    for h in 1:nh
        @constraint(ph,sum(x[y,hp] for hp=1:nh if (hp!=h) && (hs[hp,:DATE_OF_EXIT_HOMELESSNESS]<=hs[h,:DATE_OF_EXIT_HOMELESSNESS]) )==z[y,h]);
    end
end
for h in 1:nh
    for yp in 1:ny
        for y in 1:ny
            if yp!=y
                @constraint(ph,hs[h,:TYPE_OF_EXIT_RRH]*(beta_h[1]+beta_y_RRH[1]*ys[y,:X17_OR_YOUNGER1]+beta_y_RRH[2]*ys[y,:WHERE_SLEEPcouch]+beta_y_RRH[3]*ys[y,:WHERE_SLEEPoutdoors]+beta_y_RRH[4]*ys[y,:WHERE_SLEEPshelter]+beta_y_RRH[5]*ys[y,:WHERE_SLEEPtransitional]+beta_y_RRH[6]*ys[y,:GET_ANY_MONEY_LEGAL_OR_OTHERy]+beta_y_RRH[7]*ys[y,:DISABILITYy]+beta_y_RRH[8]*ys[y,:nst])+hs[h,:TYPE_OF_EXIT_PSH]*(beta_h[2]+beta_y_PSH[1]*ys[y,:X17_OR_YOUNGER1]+beta_y_PSH[2]*ys[y,:WHERE_SLEEPcouch]+beta_y_PSH[3]*ys[y,:WHERE_SLEEPoutdoors]+beta_y_PSH[4]*ys[y,:WHERE_SLEEPshelter]+beta_y_PSH[5]*ys[y,:WHERE_SLEEPtransitional]+beta_y_PSH[6]*ys[y,:GET_ANY_MONEY_LEGAL_OR_OTHERy]+beta_y_PSH[7]*ys[y,:DISABILITYy]+beta_y_PSH[8]*ys[y,:nst])-hs[h,:TYPE_OF_EXIT_RRH]*(beta_h[1]+beta_y_RRH[1]*ys[yp,:X17_OR_YOUNGER1]+beta_y_RRH[2]*ys[yp,:WHERE_SLEEPcouch]+beta_y_RRH[3]*ys[yp,:WHERE_SLEEPoutdoors]+beta_y_RRH[4]*ys[yp,:WHERE_SLEEPshelter]+beta_y_RRH[5]*ys[yp,:WHERE_SLEEPtransitional]+beta_y_RRH[6]*ys[yp,:GET_ANY_MONEY_LEGAL_OR_OTHERy]+beta_y_RRH[7]*ys[yp,:DISABILITYy]+beta_y_RRH[8]*ys[yp,:nst])-hs[h,:TYPE_OF_EXIT_PSH]*(beta_h[2]+beta_y_PSH[1]*ys[yp,:X17_OR_YOUNGER1]+beta_y_PSH[2]*ys[yp,:WHERE_SLEEPcouch]+beta_y_PSH[3]*ys[yp,:WHERE_SLEEPoutdoors]+beta_y_PSH[4]*ys[yp,:WHERE_SLEEPshelter]+beta_y_PSH[5]*ys[yp,:WHERE_SLEEPtransitional]+beta_y_PSH[6]*ys[yp,:GET_ANY_MONEY_LEGAL_OR_OTHERy]+beta_y_PSH[7]*ys[yp,:DISABILITYy]+beta_y_PSH[8]*ys[yp,:nst])==vp[y,yp,h]-vn[y,yp,h]);
            end
        end
    end
end   
for y in 1:ny
    for yp in 1:ny
        for h in 1:nh
            if yp!=y
                if rho[1,y]>rho[1,yp]
                    @constraint(ph,vp[y,yp,h]+vn[y,yp,h]>=ep*(1-u[y,yp,h]));
                elseif rho[1,y] < rho[1,yp]
                    @constraint(ph,vp[y,yp,h]+vn[y,yp,h]>=ep*u[y,yp,h]);
                end
            end
        end
    end
end
for y in 1:ny
    for yp in 1:ny
        for h in 1:nh
            if yp!=y
		@constraint(ph,vp[y,yp,h]<=M*u[y,yp,h]);
		@constraint(ph,vn[y,yp,h]<=M*(1-u[y,yp,h]) );
		@constraint(ph,z[y,h]+(1-u[y,yp,h])+w[y,yp,h]>=1);
		@constraint(ph,z[y,h]+(1-z[yp,h])+w[y,yp,h]>=1);
                @constraint(ph,w[y,yp,h]==1-w[yp,y,h]);
                @constraint(ph,w[y,yp,h]>=x[y,h]);
            end
        end
    end
end
for y in 1:ny
    for h in 1:nh 
        if ys[y,:arrivedate]<=hs[h,:DATE_OF_EXIT_HOMELESSNESS]
            @constraint(ph,1-z[y,h]<=sum(x[yp,h] for yp=1:ny if ys[yp,:arrivedate]<=hs[h,:DATE_OF_EXIT_HOMELESSNESS]));
        end
        if ys[y,:arrivedate]>hs[h,:DATE_OF_EXIT_HOMELESSNESS]
            @constraint(ph,x[y,h]==0);
        end                                                            
    end
end

ny_b=zeros(1,3);
ny_b[1]=size(ys[(ys[:nst].>=0) .& (ys[:nst].<=3),:])[1]
ny_b[2]=size(ys[(ys[:nst].>=4) .& (ys[:nst].<=7),:])[1]
ny_b[3]=size(ys[(ys[:nst].>=8),:])[1]

ncons_ph=3
@constraintref cons_ph[1:ncons_ph];
cons_ph[1] = @constraint(ph, sum(sum(p[y,h]*x[y,h] for h=1:nh)for y=1:ny if (ys[y,:nst]>=0 && ys[y,:nst]<=3))+sum(p_go_nh[y]*p_nh[y] for y=1:ny if (ys[y,:nst]>=0 && ys[y,:nst]<=3))-sum(sum(p_go_nh[y]*p_nh[y]*x[y,h] for h=1:nh) for y=1:ny if (ys[y,:nst]>=0 && ys[y,:nst]<=3)) == ny_b[1]*(e+ei[1]) );
cons_ph[2] = @constraint(ph, sum(sum(p[y,h]*x[y,h] for h=1:nh)for y=1:ny if (ys[y,:nst]>=4 && ys[y,:nst]<=7))+sum(p_go_nh[y]*p_nh[y] for y=1:ny if (ys[y,:nst]>=4 && ys[y,:nst]<=7))-sum(sum(p_go_nh[y]*p_nh[y]*x[y,h] for h=1:nh) for y=1:ny if (ys[y,:nst]>=4 && ys[y,:nst]<=7))== ny_b[2]*(e+ei[2]) );
cons_ph[3] = @constraint(ph, sum(sum(p[y,h]*x[y,h] for h=1:nh)for y=1:ny if ys[y,:nst]>=8)+sum(p_go_nh[y]*p_nh[y] for y=1:ny if ys[y,:nst]>=8)-sum(sum(p_go_nh[y]*p_nh[y]*x[y,h] for h=1:nh) for y=1:ny if ys[y,:nst]>=8) == ny_b[3]*(e+ei[3]) );              

###################
solve(ph)

ph.solver.options[1]=(:TimeLimit, 5000)


q=getdual(cons_ph) 
getvalue(beta_y_RRH)
getvalue(beta_y_PSH)
getvalue(beta_h)

##################################
#open("phout.txt","w") do fp
 #   println(fp, "Objective value: ", getobjectivevalue(ph))
    #println(fp, "beta_y = ", getvalue(beta_y))
   # println(fp, "beta_h = ", getvalue(beta_h*10000))
#end
#writeLP(ph,"ph1.lp")
#writeMPS(ph,"ph.mps")
#ys
#hs
#p
#getvalue(x)
#getvalue(w)                                                                                        
#getvalue(vp)                                                                              
