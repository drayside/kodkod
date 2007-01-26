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

#include "Solver.h"

#include <jni.h>
#include "kodkod_engine_satlab_MiniSatProver.h"

/*
 * Class:     kodkod_engine_satlab_MiniSatProver
 * Method:    make
 * Signature: ()J
 */
JNIEXPORT jlong JNICALL Java_kodkod_engine_satlab_MiniSatProver_make
  (JNIEnv *, jclass) {
  PSolver* solver = new PSolver();
  solver->proof = new Proof();
  //cout << "Created new minisat solver " << solver << endl;
  //cout << "new solver has " << solver->nVars() << " variables" << endl;
  return ((jlong) solver);
}

/*
 * Class:     kodkod_engine_satlab_MiniSatProver
 * Method:    free
 * Signature: (J)V
 */
JNIEXPORT void JNICALL Java_kodkod_engine_satlab_MiniSatProver_free
  (JNIEnv *, jobject, jlong solver) {
  //cout << "destructing minisat solver " << solver << endl;
  PSolver* solverPtr = (PSolver*)solver;
  delete solverPtr->proof;
  solverPtr->proof = NULL;
  delete solverPtr;  
}

/*
 * Class:     kodkod_engine_satlab_MiniSatProver
 * Method:    addVariables
 * Signature: (JI)V
 */
JNIEXPORT void JNICALL Java_kodkod_engine_satlab_MiniSatProver_addVariables
  (JNIEnv *, jobject, jlong solver, jint  numVars) {
  PSolver* solverPtr = (PSolver*) solver;
  //cout << "minisat solver " << solver << " has " << solverPtr->nVars() << " variables" << endl; 
  //cout << "adding " << numVars << " variables to minisat solver " << solver << endl;
  for(int i = 0; i < numVars; ++i) {
     solverPtr->newVar();
  }
}

/*
 * Class:     kodkod_engine_satlab_MiniSatProver
 * Method:    addClause
 * Signature: (J[I)V
 */
JNIEXPORT void JNICALL Java_kodkod_engine_satlab_MiniSatProver_addClause
  (JNIEnv * env, jobject, jlong solver, jintArray clause) {
    jsize length = env->GetArrayLength(clause);
    jint* buf = env->GetIntArrayElements(clause, JNI_FALSE);
    vec<Lit> lits;
    for(int i = 0; i < length; ++i) {
        int var = *(buf+i);
        lits.push((var > 0) ? Lit(var-1) : ~Lit(-var-1));
    }
    ((PSolver*)solver)->addClause(lits);
    env->ReleaseIntArrayElements(clause, buf, 0);
 }

/*
 * Class:     kodkod_engine_satlab_MiniSatProver
 * Method:    solve
 * Signature: (J)Z
 */
JNIEXPORT jboolean JNICALL Java_kodkod_engine_satlab_MiniSatProver_solve
  (JNIEnv *, jobject, jlong solver) {
   return ((PSolver*)solver)->solve();
  }

/*
 * Class:     kodkod_engine_satlab_MiniSatProver
 * Method:    valueOf
 * Signature: (JI)Z
 */
JNIEXPORT jboolean JNICALL Java_kodkod_engine_satlab_MiniSatProver_valueOf
  (JNIEnv *, jobject, jlong solver, jint var) {
  return ((PSolver*)solver)->model[var-1]==l_True;
 }

/*
 * Class:     kodkod_engine_satlab_MiniSatProver
 * Method:    saveProof
 * Signature: (JLjava/lang/String;)V
 */
JNIEXPORT jint JNICALL Java_kodkod_engine_satlab_MiniSatProver_saveProof
  (JNIEnv * env, jobject obj, jlong solver, jstring file) {
  
  jboolean iscopy;
  const char *filename = env->GetStringUTFChars(file, &iscopy);
  if (filename == NULL) {
      return -1; /* OutOfMemoryError already thrown */
  }
  Proof* proof = ((PSolver*)solver)->proof;
  proof->save(filename);
  env->ReleaseStringUTFChars(file, filename);
  return proof->last(); 
}
