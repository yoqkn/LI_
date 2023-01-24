#include <iostream>
#include <stdlib.h>
#include <algorithm>
#include <vector>
using namespace std;

#define UNDEF -1
#define TRUE 1
#define FALSE 0

uint numVars;
uint numClauses;
vector<vector<int> > clauses;
vector<int> model;
vector<int> modelStack;
vector<vector<int>> occurListPos;
vector<vector<int>> occurListNeg;
vector<double> counterList;
vector<int> freqList;
uint indexOfNextLitToPropagate;
uint decisionLevel;


void readClauses( ){
  // Skip comments
  char c = cin.get();
  while (c == 'c') {
    while (c != '\n') c = cin.get();
    c = cin.get();
  }  
  // Read "cnf numVars numClauses"
  string aux;
  cin >> aux >> numVars >> numClauses;
  clauses.resize(numClauses);
  occurListPos.resize(numVars);
  occurListNeg.resize(numVars);
  counterList.resize(numVars, 0.0);
  freqList.resize(numVars, 0);

  // Read clauses
  for (uint i = 0; i < numClauses; ++i) {
    int lit;
    while (cin >> lit and lit != 0) {
      cout << i << "-" << lit << endl;
        clauses[i].push_back(lit);
        ++freqList[abs(lit)];

        if (lit > 0) occurListPos[lit].push_back(i);
        else occurListNeg[-lit].push_back(i);
    }
  }    
}



int currentValueInModel(int lit){
  if (lit >= 0) return model[lit];
  else {
    if (model[-lit] == UNDEF) return UNDEF;
    else return 1 - model[-lit];
  }
}


void setLiteralToTrue(int lit){
  modelStack.push_back(lit);
  if (lit > 0) model[lit] = TRUE;
  else model[-lit] = FALSE;		
}

void DivideVectorByScalar(vector<double> &v, double k){
   for(uint i=0;i<v.size();++i)
       v[i] = v[i] / k;
}

bool propagateGivesConflict ( ) {

  //MILLORA: Nomes comprovar per aquells clausules que tenen elements negats
  /*while ( indexOfNextLitToPropagate < modelStack.size() ) {
    ++indexOfNextLitToPropagate;
    for (uint i = 0; i < numClauses; ++i) {
      bool someLitTrue = false;
      int numUndefs = 0;
      int lastLitUndef = 0;
      for (uint k = 0; not someLitTrue and k < clauses[i].size(); ++k){
	int val = currentValueInModel(clauses[i][k]);
	if (val == TRUE) someLitTrue = true;
	else if (val == UNDEF){ ++numUndefs; lastLitUndef = clauses[i][k]; }
      }
      if (not someLitTrue and numUndefs == 0) return true; // conflict! all lits false
      else if (not someLitTrue and numUndefs == 1) setLiteralToTrue(lastLitUndef);	//Si hi ha un unica variable indefinida es dona lloc la propagació on configura literal sigui cert
    }
  }
  return false;
  */
  while ( indexOfNextLitToPropagate < modelStack.size() ) {
    int litToPropagate = modelStack[indexOfNextLitToPropagate];  //ultim propagat
    ++indexOfNextLitToPropagate;
    //Si és negatiu cal comprovar per aquelles clàusules que continguin litToPropagate amb signe positiu que la clàusula sigui certa
    vector<int> clausesLit;
    if (litToPropagate < 0) clausesLit =  occurListPos[abs(litToPropagate)];
    else vector<int> clausesLit =  occurListNeg[litToPropagate];
        for (uint i = 0; i < clausesLit.size(); ++i) {  //vector que conté totes les clàusules on apareix negat de litToPropagate
            bool someLitTrue = false;
            int numUndefs = 0;
            int lastLitUndef = 0;
            int clause = clausesLit[i];
            for (uint j = 0; not someLitTrue and j < clauses[clause].size(); ++j) {
                int val = currentValueInModel(clauses[clause][j]);
                if (val == TRUE) someLitTrue = true;
                else if(val == UNDEF) {
                    ++numUndefs;
                    lastLitUndef = clauses[clause][j];

                }
            }
            if (not someLitTrue and numUndefs == 0) {
                for (uint j = 0; j < clauses[clause].size(); ++j) {
                    ++counterList[clauses[clause][j]];
                    if (counterList[clauses[clause][j]] > 20) DivideVectorByScalar(counterList, 20.0);
                }
            return true;
            }
            else if (not someLitTrue and numUndefs == 1) setLiteralToTrue(lastLitUndef);
        }
    }
    return false;
}


void backtrack(){
  uint i = modelStack.size() -1;
  int lit = 0;
  while (modelStack[i] != 0){ // 0 is the DL mark
    lit = modelStack[i];
    model[abs(lit)] = UNDEF;
    modelStack.pop_back();
    --i;
  }
  // at this point, lit is the last decision
  modelStack.pop_back(); // remove the DL mark
  --decisionLevel;
  indexOfNextLitToPropagate = modelStack.size();
  setLiteralToTrue(-lit);  // reverse last decision
}


// Heuristic for finding the next decision literal:
int getNextDecisionLiteral(){//Triar el següent literal no assignat per assignar-li un 1 <<< Es pot millorar
 /* int num = 0;
  int nextDL = 0;
*/
    int nextDL = 0;
    int nTimes = 0;
    for (uint i = 1; i <= numVars; ++i) {
        if (model[i] == UNDEF) {    //escoll aquell que apareix més vegades en els literals indefinits
            if (counterList[i] > nTimes) {
                nTimes = counterList[i];
                nextDL = i;
            }
            /*if (freqList[i] > nTimes) {
                nTimes = freqList[i];
                nextDL = i;
            }*/
        }

    }
    return nextDL;
        // stupid heuristic:
 /*   if (model[i] == UNDEF)  {
      if (int(occurList[i].size()) > num)  {
        nextDL = i;  // returns first UNDEF var, positively
        num = int(occurList[i].size());
      }
    }
    return i;
  }
  return 0; // reurns 0 when all literals are defined
  */
}

void checkmodel(){
  for (uint i = 0; i < numClauses; ++i){
    bool someTrue = false;
    for (uint j = 0; not someTrue and j < clauses[i].size(); ++j)
      someTrue = (currentValueInModel(clauses[i][j]) == TRUE);
    if (not someTrue) {
      cout << "Error in model, clause is not satisfied:";
      for (uint j = 0; j < clauses[i].size(); ++j) cout << clauses[i][j] << " ";
      cout << endl;
      exit(1);
    }
  }  
}

int main(){ 
  readClauses(); // reads numVars, numClauses and clauses
  model.resize(numVars+1,UNDEF);
  indexOfNextLitToPropagate = 0;  
  decisionLevel = 0;
  
  // Take care of initial unit clauses, if any
  for (uint i = 0; i < numClauses; ++i)
    if (clauses[i].size() == 1) {
      int lit = clauses[i][0];
      int val = currentValueInModel(lit);
      if (val == FALSE) {cout << "UNSATISFIABLE" << endl; return 10;}
      else if (val == UNDEF) setLiteralToTrue(lit);
    }
  
  // DPLL algorithm
  while (true) {
    ///mira de fer una progapació
    while ( propagateGivesConflict() ) {  //Si una propagació dona conflicte -> backtracking
      if ( decisionLevel == 0) { cout << "UNSATISFIABLE" << endl; return 10; }
      backtrack();
    }
    int decisionLit = getNextDecisionLiteral();
    if (decisionLit == 0) { checkmodel(); cout << "SATISFIABLE" << endl; return 20; }
    // start new decision level:
    modelStack.push_back(0);  // push mark indicating new DL
    ++indexOfNextLitToPropagate;
    ++decisionLevel;
    setLiteralToTrue(decisionLit);    // now push decisionLit on top of the mark
  }
}  

//millorar getNextDecisionLiteral, i utilitza occurList(listat on indica els varibles a quines clausules apreixen)

