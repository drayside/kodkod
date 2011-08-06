#include <iostream>
#include <cstdlib>
#include <cstdio>
#include <cstring>
#include <cassert>

#include <map>
#include <set>
#include <vector>
#include <list>
#include <string>
#include <algorithm>
#include <queue>

using namespace std;

#include "core/zchaff_solver.h"

#include <jni.h>
#include "kodkod_engine_satlab_ZChaffMincost.h"

JNIEXPORT jlong JNICALL Java_kodkod_engine_satlab_ZChaffMincost_make
  (JNIEnv *, jclass) {
	MSolver* solver = new MSolver();
	return ((jlong) solver);
}

JNIEXPORT void JNICALL Java_kodkod_engine_satlab_ZChaffMincost_free
  (JNIEnv *, jobject, jlong solver) {
    delete ((MSolver*)solver);
}


JNIEXPORT void JNICALL Java_kodkod_engine_satlab_ZChaffMincost_setCost
  (JNIEnv *, jobject, jlong solver, jint variable, jint cost) {
	((MSolver*)solver)->set_variable_cost(variable,cost);
}

JNIEXPORT jint JNICALL Java_kodkod_engine_satlab_ZChaffMincost_costOf
  (JNIEnv *, jobject, jlong solver, jint variable) {
  return ((MSolver*)solver)->variable(variable).cost();
}
  
JNIEXPORT void JNICALL Java_kodkod_engine_satlab_ZChaffMincost_addVariables
  (JNIEnv *, jobject, jlong solver, jint numVars) {
  MSolver* solverPtr = (MSolver*) solver;
  if (solverPtr->num_variables()==0) {
   solverPtr->set_variable_number(numVars);
  } else {
   for(int i = 0; i < numVars; ++i) {
     solverPtr->add_variable();
   }
  }
}

JNIEXPORT jboolean JNICALL Java_kodkod_engine_satlab_ZChaffMincost_addClause
  (JNIEnv * env, jobject, jlong solver, jintArray clause) {
    jsize length = env->GetArrayLength(clause);
    jint* buf = env->GetIntArrayElements(clause, JNI_FALSE);
    for(int i = 0; i < length; ++i) {
        int var = *(buf+i);
        *(buf+i) = var < 0 ? (-var << 1) | 1 : var << 1;
    }
    MClauseIdx clauseIdx = ((MSolver*)solver)->add_orig_clause((int*)buf, length);
    env->ReleaseIntArrayElements(clause, buf, 0);
    return clauseIdx>=0;
}


  
JNIEXPORT jboolean JNICALL Java_kodkod_engine_satlab_ZChaffMincost_solve
  (JNIEnv *, jobject, jlong solver) {
  int status = ((MSolver*)solver)->solve();
  assert (status == UNSATISFIABLE || status == SATISFIABLE);
  return (status == SATISFIABLE);
}

JNIEXPORT jboolean JNICALL Java_kodkod_engine_satlab_ZChaffMincost_valueOf
  (JNIEnv *, jobject, jlong solver, jint literal) {
  return ((MSolver*)solver)->variable(literal).value()==1;
}
