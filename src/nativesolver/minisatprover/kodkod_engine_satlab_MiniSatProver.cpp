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
  (JNIEnv *env, jclass) {
  PSolver* solver = new PSolver();
  solver->proof = new Proof();
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
 * Signature: (J[I)Z
 */
JNIEXPORT jboolean JNICALL Java_kodkod_engine_satlab_MiniSatProver_addClause
  (JNIEnv * env, jobject, jlong solver, jintArray clause) {
    jsize length = env->GetArrayLength(clause);
    jint* buf = env->GetIntArrayElements(clause, JNI_FALSE);
    PSolver* solverPtr = ((PSolver*)solver);
    vec<Lit> lits;
    for(int i = 0; i < length; ++i) {
        int var = *(buf+i);
        lits.push((var > 0) ? Lit(var-1) : ~Lit(-var-1));
    }
    int clauseId = solverPtr->proof->next();
    solverPtr->addClause(lits);
    env->ReleaseIntArrayElements(clause, buf, 0);
    return clauseId < solverPtr->proof->next();
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


struct TraceGenerator : public ProofTraverser {
	JNIEnv* env;
	jobjectArray trace;
	jboolean core;
	jlong* learned;
	int idx;
	
	TraceGenerator(JNIEnv* environment, jboolean recordCore, jint goal) { 
	  	idx = 0; 
	  	env = environment;
	  	core = recordCore;
	  	trace = (jobjectArray)env->NewObjectArray(goal+2, env->FindClass("java/lang/Object"), NULL);
	  	
	  	jlongArray last = env->NewLongArray((goal>>6)+1);
	  	env->SetObjectArrayElement(trace, goal+1, last);
	  	
	  	learned = env->GetLongArrayElements(last, JNI_FALSE);
	  	env->DeleteLocalRef(last);
	  	
	}
	
	void root (const vec<Lit>& c) {
		if (core) {
			
			jintArray lits = env->NewIntArray(c.size());
			jint* data = env->GetIntArrayElements(lits, JNI_FALSE);
			for(int i = 0; i < c.size(); i++) {
				data[i] = toDimacs(c[i]);
			}
			env->ReleaseIntArrayElements(lits, data, 0); 
			env->SetObjectArrayElement(trace, idx, lits);
			env->DeleteLocalRef(lits);
		}
		idx++;
	}
	
	 void chain  (const vec<ClauseId>& cs, const vec<Var>& xs) {
        learned[(idx>>6)] |= ((jlong)(((jlong)1)<<(idx&0x3F)));
        jintArray ante = env->NewIntArray(cs.size());
        jint* data = env->GetIntArrayElements(ante, JNI_FALSE);
        for(int i = 0; i< cs.size(); i++) {
        	data[i] = cs[i];
        }
        env->ReleaseIntArrayElements(ante, data, 0);
        env->SetObjectArrayElement(trace, idx, ante);
        env->DeleteLocalRef(ante);
		idx++;
	}
	
	void deleted(ClauseId c) {}
	
	void done() {
		env->ReleaseLongArrayElements((jlongArray)(env->GetObjectArrayElement(trace, idx)), learned, 0);
	}

};

/*
 * Class:     kodkod_engine_satlab_MiniSatProver
 * Method:    trace
 * Signature: (JZ)[Ljava/lang/Object;
 */
JNIEXPORT jobjectArray JNICALL Java_kodkod_engine_satlab_MiniSatProver_trace
  (JNIEnv * env, jobject, jlong solver, jboolean recordCore) {
    Proof* proof = ((PSolver*) solver)->proof;
  	TraceGenerator tgen = TraceGenerator(env, recordCore, proof->last());
    proof->traverse(tgen);
    return tgen.trace;
  }

