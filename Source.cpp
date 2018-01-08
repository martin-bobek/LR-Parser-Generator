#include <algorithm>
#include <fstream>
#include <functional>
#include <iostream>
#include <memory>
#include <string>
#include <vector>
#include <cctype>

#define EPSILON		nullptr		// an the pointer to an Epsilon symbol. nullptr is used since it cannot ever match the pointer to a real symbol

// try to restrict access as much as possible
// try to remove as many move's as possible (remember to write both a default move constructor and assignment in all relevant classes!).
// add as many noexcepts as possible

using std::move;
using std::ostream;
using std::istream;
using std::string;
using std::vector;
using std::unique_ptr;
using std::reference_wrapper;

class Production;
class Terminal;
class NonTerminal;
class State;
class NFA;
class Action;
class Shift;
class Reduce;
typedef unique_ptr<Action> pAction;
typedef unique_ptr<Reduce> pReduce;
typedef unique_ptr<State> pState;
typedef unique_ptr<NonTerminal> pNonTerminal;
typedef unique_ptr<Terminal> pTerminal;
typedef reference_wrapper<NFA> rNFA;

vector<Terminal *> SetUnion(const vector<Terminal *> &setA, const vector<Terminal *> &setB);	// computes the union of two sets of pointers. assumes setA, setB are in increasing order. Result is then also ordered
void FCopy(ostream &os, istream &is);

class Symbol
{
public:
	virtual ~Symbol() = 0;
	virtual bool Nullable() const = 0;			// exposes nullable field (read only)
	virtual vector<Terminal *> First() = 0;		// exposes the first set of the symbol (read only)
	virtual void ExtractFollowConstraints(const NonTerminal *lhs, vector<Symbol *>::const_iterator it, vector<Symbol *>::const_iterator end) = 0; // adds the constraints on the follow set of this symbol for a production. lhs points to the nonterminal on the lhs of a production, it points to the next symbol
	virtual vector<const State *> NfaRoots() = 0;	// returns a vector of pointers to the first states of all the NFA's which reduce to this symbol
	string Name() const { return name; }

	virtual void AddShiftGo(size_t from, size_t to) = 0;	// Add a shift or go transition on this symbol from state "from" to state "to"
protected:
	Symbol(string &&name) : name(move(name)) {}
	string name;										// should this be private
};
class NFA
{
public:
	NFA() = default;			// creates empty nfa object
	void Initialize(Production &production, const NonTerminal *nonTerminal, size_t numProduction); // generates a string of nfa states which represent (reduce to) a production. This includes recursively adding epsilon transitions to NFA's which reduce to nonterminals in this production. nonTerminal is a pointer to the LHS of the production, and numProduction identifies which production of nonTerminal this is
	NFA(const NFA &) = delete;
	NFA(NFA &&rhs) = default;
	NFA &operator=(const NFA &) = delete;
	NFA &operator=(NFA &&rhs) = default;
	bool Empty() const { return states.empty(); }	// returns true if the NFA states have not yet been created (the NFA has just been defualt constructed)
	const State *GetRoot() { return states[0].get(); }	// returns a pointer to the first state of nfa. Assumes the nfa has already been initialized
	static NFA Merge(vector<vector<NFA>> &&nfas);		// Takes all the pieces of the final nfa (each piece represents a production) (these pieces are already fully interconnected with transitions) and merges them into a single NFA with ownership of all the states
	void AddReductions(size_t nfaState, size_t dfaState, std::ostream &out);	// checks to see if the nfaState (in the dfaState subset) is a reduce state, and if so adds adds a reduction transition on the appropriate terminals
	size_t Size() const { return states.size(); }		// returns the number of states in the NFA
	vector<bool> &Closure(vector<bool> &subset) const;	// modifies subset (passed in by reference) to be the epsilon closure of the passed in set. Also returns a reference to the modified set
	vector<bool> Move(const vector<bool> &subset, const Symbol *symbol) const;
	vector<size_t> ReductionStates() const;				// returns a vector with the state id's (in ascending order) of all the states which are reduce states
private:
	void closureRecursion(size_t current, vector<bool> &checked, vector<bool> &subset) const;	// marks current as part of subset checked (only if the state is not already marked in checked). Then it calls closureRecursion on all the states which can be reached by a single epsilon transition from current
	vector<pState> states;
};
class Production
{
public:
	bool ComputeNullable() const;				// returns true if production is nullable (all RHS symbols are nullable)
	vector<Terminal *> ComputeFirst() const;	// returns the first set of the production
	void FollowConstraints(const NonTerminal *lhs) const;  // evaluates the preliminary constraints on follow sets of all the nonterminals in the production. lhs points to the nonterminal on the lhs of the production

	NFA MoveNfa() { return move(nfa); }		// extracts NFA without copying when individual production nfa's are being assembled into the complete nfa
	const State *GetRoot(const NonTerminal *nonTerminal, size_t production);	// Gets a pointer to the first state of the NFA for the production
	size_t Size() const { return symbols.size(); }							// number of symbols on RHS of production
	Symbol *operator[](size_t index) const { return symbols[index]; }		// returns pointer to a symbol on the RHS of production

	void PrintDescription(ostream &os) const;
	void PrintClass(ostream &os, const string name, size_t num) const;
private:
	vector<Symbol *> symbols;								// symbols on the RHS of the production, in order
	NFA nfa;												// Linear nfa with transitions on the symbols on RHS of production
public:
	Production(vector<Symbol *> &&symbols) : symbols(move(symbols)) {}	// moves the vector of RHS symbols into the newly constructed object
	size_t PrintReduce(ostream &os) const;
};
class NonTerminal : public Symbol
{
public:
	NonTerminal(string &&name, bool acceptor = false) : Symbol(move(name)), acceptor(acceptor) {}
	bool Nullable() const { return nullable; };		// exposes nullable field (read only)
	bool NullableSetup();	// sets the nullable field to true if any productions are nullable. returns true if the nullable field was not changed
	vector<Terminal *> First() { return first; }	// exposes the first set of the nonterminal (read only)
	bool FirstSetup();		// sets the first sets to the unions of the first sets of all the productions. returns true if the first set is not changed
	vector<Terminal *> Follow() const { return follow; }	// exposes the follow set of the nonterminal (read only)
	void ExtractFollowConstraints(const NonTerminal *lhs, vector<Symbol *>::const_iterator it, vector<Symbol *>::const_iterator end); // adds the constraints on the follow set of this symbol for a production. lhs points to the nonterminal on the lhs of a production, it points to the next symbol
	void SetupFollowConstraints() const;		// evaluates the prelinary follow set constraints for the symbols on the lhs's of all the productions for this nonterminal
	void MergeFollowConstraints();				// removes duplicates from the followConstraints set
	bool SatisfyFollowConstraints();			// uses the constraints previously set up to complete the follow sets. returns true if the follow set hasnt changed on this iteration
	vector<const State *> NfaRoots();			// returns a vector of pointers to the first states of all the NFA's which reduce to this nonTerminal
	vector<NFA> GetBranches();
	void StartNfa() { productions[0].GetRoot(nullptr, 0); }	// starts the recursive process of generating the individual production NFA's. The single production for the terminated nonterminal is used to generate the first NFA which recursively links to all other NFA's. A link to the terminated nonterminal production is not used, so is not provided in the parameters

	void PrintClass(ostream &os) const;
private:
	bool nullable = false;					// field indicating that a nonterminal is nullable
	vector<Terminal *> first;				// a set of all the terminals in the first set
	vector<Terminal *> follow;				// a set of all the terminals in the follow set
	vector<const NonTerminal *> followConstraints; // points to all the nonTerminals, whos follow sets must be contained in this nonterminals follow set
	vector<Production> productions;			// a vector of all the productions with the nonterminal on its LHS
	vector<size_t> gos;
	bool acceptor;
public:
	void AddProduction(vector<Symbol *> &&production); // creates a new production from a vector of pointers to the symbols on the RHS of the production, and inserts it into the productions vector
	void PrintActions(ostream &os) const;
	void PrintProductionDescription(ostream &os, size_t production) const;
	void PrintReduce(ostream &os, size_t production) const;
	void PrepareGos(size_t numStates) { gos.resize(numStates); }	// gos should have the same size as number of states (gos[i] represents transition taken in the ith state). gos is filled with 0's, representing no transition
	void AddShiftGo(size_t from, size_t to) { gos[from] = to + 1; }	// the "on" symbol was a nonterminal, so the transition is a go. The entry in gos indicates that this nonTerminal is read, and the current state is "from", then a transition should happen to the state "to". 0 is reserved for no transition, so the state number + 1 is used to indicate a transition
};
class Terminal : public Symbol
{
public:
	Terminal(string &&name) : Symbol(move(name)) {}
	bool Nullable() const { return false; }				// a terminal is never nullable
	vector<Terminal *> First() { return{ this }; }		// the first set of a terminal is itself
	void ExtractFollowConstraints(const NonTerminal *, vector<Symbol *>::const_iterator, vector<Symbol *>::const_iterator) {} // the follow sets of terminals are unimportant and not computed
	vector<const State *> NfaRoots() { return vector<const State *>(); }	// returns an empty vector since no NFA's reduce to a terminal
	void PrintActions(ostream &os, bool isEnd = false) const;

	void PrepareActions(size_t numStates) { actions.resize(numStates); }	// actions (shift/reduce) should have the same size as number of states (actions[i] represents the transition taken in the ith state). actions is filled with nullptr's, representing no transitions
	void AddShiftGo(size_t from, size_t to);			// the "on" symbol was a terminal, so the transition is a shift. the appropriate shift object is added to actions
	void AddReduction(size_t from, pReduce &&reduce, std::ostream &out);	// Checks to see if there is a conflict on the from state (another shift or reduction already exists on the from state). If there is a conflict, a precedence rule to resolve it is requested. The Reduce is then added (or not added) to the actions vector at the from state
private:
	vector<pAction> actions;
};
class State
{
public:
	State(const NonTerminal *nonTerminal = nullptr, size_t production = 0) : nonTerminal(nonTerminal), production(production) {} // Default constructs a non reduce state (nonTerminal is nullptr) if no arguments provided. Arguments used to construct a reduce state with a link to the production
	void AddTransition(const Symbol *symbol, const State *to) { transitions.push_back({ symbol, to }); } // Adds a transition, specifying the symbol on which the transition happens, and the state which is transitioned to 
	void AddReductions(size_t dfaState, std::ostream &out);	// checks to see if the nfaState (in the dfaState subset) is a reduce state, and if so adds adds a reduction transition on the appropriate terminals
	void AddNumber(size_t numState) { id = numState; };	// gives each state a unique number (in a consecutive range). These id's are used when the nfa is transformed into a dfa
	vector<size_t> TransitionList(const Symbol *symbol) const;		// returns a list of the id's of states which can be reached by a single transition on the symbol from the current state
	bool Reducible() const { return nonTerminal ? true : false; }	// Returns true if a nonTerminal can be reduced from this state (nonTerminal points to this nonterminal). Otherwise returns false (nonTerminal = nullptr)
private:
	struct Transition		// representation of NFA state transition
	{
		const Symbol * symbol;		// symbol on which the transition occurs
		const State * to;			// pointer to the state which is transitioned to
	};
	vector<Transition> transitions;		// vector of all the possible transitions for the state
	const NonTerminal *nonTerminal;		// if the state is the end of a production (reduce state), this points the nonterminal on the LHS of the production. If the state is not a reduce state, this is null
	size_t production;				// if the state is a reduce state, this gives the number of the production for the nonterminal
	size_t id;			// each state is given a unique number (in a consecutive range). These id's are used when the nfa is transformed into a dfa
};
class Grammer
{
public:
	static Grammer GetGrammer(std::ostream &os);	// Obtains productions from command line inputs and constructs a fully formed grammer, including an END symbol and the two head nonterminals (terminated and acceptor)
	void InitializeNullable();		// computes which nonterminals can produce just a empty string of symbols and marks this in a nullable field
	void InitializeFirst();			// computes which terminals can be the first element of a string produced by a nonterminal
	void InitializeFollow();		// computes which terminals can directly follow a nonTerminal (including END terminal)
	void GenerateDfa(std::ostream &out);
	
	void Print(ostream &os, istream &iClass, istream &iTerminals, istream &iDefinitions) const;
private:
	Grammer();	// sets up the head of the grammer: terminated -> acceptor end; acceptor -> start symbol of user grammer
	class DFA
	{
	public:
		DFA(NFA &nfa) : nfa(nfa) {}
		DFA(DFA &&) = default;
		DFA &operator=(DFA &&) = default;
		static DFA Generate(NFA &nfa, const Grammer &grammer);	// takes an nfa and generates an equivalent dfa, omitting transitions on the terminated and accepting nonterminals. a pointer to grammer is needed in order to access terminals and nonterminals
		static DFA Optimize(const DFA &dfa);
		void CreateActions(Grammer &grammer, std::ostream &out) const;	// Analyzes the dfa and records all the go, reduce, and shift transitions inside the terminal and nonTerminal objects, asking for precedence rules when conflicts are found. After this, the terminals and nonterminal objects contain enough information to write the Parser program
	private:
		static bool isNonempty(const vector<bool> &subset);
		vector<vector<bool>> states;	// contains subsets of the nfa states, representing the states of the dfa. Subsets of nfa states are represented by a vector where vec[i] = ith state is part of the set?
		struct Transition			// representation of NFA state transitions
		{
			Symbol *on;				// a pointer to the symbol on which the state transition happens
			size_t to;				// the id of the state which is transitioned to
		};
		vector<vector<Transition>> transitions;	// vector of lists of all the transitions between dfa states. transitions[which dfa state][which transition] = Transition object
		vector<vector<size_t>> reductionStates; // vector of lists of all the reductions contained by dfa states (there can be multiple). For each production reduction, there is only a single nfa state associated with it, so the nfa state id is used to uniquely identify the reduction. reductionStates[which dfa state][which reduction] = nfa state id
		rNFA nfa;
	};
	NFA nfa;
	DFA dfa;

	vector<pNonTerminal> nonTerminals;	// all the nonterminals for the grammer. This arrays has ownership of all the nonterminal objects
	vector<pTerminal> terminals;		// all the terminals for the grammer. This array has ownership of all the terminal objects
};
class Action
{
public:
	virtual ~Action() = 0;
	virtual void PrintAction(ostream &os) const = 0;
	virtual void PrintName(ostream &os) const = 0;
	virtual bool operator==(const Action &) const = 0;
	virtual bool operator==(const Shift &) const = 0;
	virtual bool operator==(const Reduce &) const = 0;
};
class Shift : public Action
{
public:
	Shift(size_t to) : to(to) {}
	void PrintAction(ostream &os) const;
	void PrintName(ostream &os) const { os << "Shift\n"; }
	bool operator==(const Action &rhs) const { return rhs == *this; }
	bool operator==(const Reduce &) const { return false; }
	bool operator==(const Shift &rhs) const { return to == rhs.to; }
private:
	size_t to;
};
class Reduce : public Action
{
public:
	Reduce(const NonTerminal *nonTerminal, size_t production) : nonTerminal(nonTerminal), production(production) {}
	void PrintAction(ostream &os) const;
	void PrintName(ostream &os) const;
	bool operator==(const Action &rhs) const { return rhs == *this; }
	bool operator==(const Shift &) const { return false; }
	bool operator==(const Reduce &rhs) const { return (nonTerminal == rhs.nonTerminal) && (production == rhs.production); }
private:
	const NonTerminal *nonTerminal;
	size_t production;
};


int main(int argc, char *argv[])
{
	if (argc != 6)
	{
		std::cerr << "Improper number of arguments entered!" << std::endl;
		return 0;
	}
	std::ofstream iGrammer(argv[5]);
	if (iGrammer.fail())
	{
		std::cerr << "Failed to open file 5!" << std::endl;
		return 1;
	}
	Grammer grammer = Grammer::GetGrammer(iGrammer); // Obtains productions from command line inputs and constructs a fully formed grammer
	grammer.InitializeNullable();			 // computes which nonterminals can produce just a empty string of symbols
	grammer.InitializeFirst();				 // computes which terminals can be the first element of a string produced by a nonterminal
	grammer.InitializeFollow();				 // computes which terminals can directly follow a nonTerminal (including END terminal)
	grammer.GenerateDfa(iGrammer);
	
	try
	{
		std::ofstream os(argv[1]);
		if (os.fail())
		{
			std::cerr << "Failed to open file 1!" << std::endl;
			return 1;
		}
		std::ifstream iClass(argv[2]);
		if (iClass.fail())
		{
			std::cerr << "Failed to open file 2!" << std::endl;
			return 1;
		}
		std::ifstream iTerminals(argv[3]);
		if (iTerminals.fail())
		{
			std::cerr << "Failed to open file 3!" << std::endl;
			return 1;
		}
		std::ifstream iDefinitions(argv[4]);
		if (iDefinitions.fail())
		{
			std::cerr << "Failed to open file 4!" << std::endl;
			return 1;
		}
		grammer.Print(os, iClass, iTerminals, iDefinitions);
	}
	catch (char *msg)
	{
		std::cerr << msg << std::endl;
	}
}

vector<Terminal *> SetUnion(const vector<Terminal *> &setA, const vector<Terminal *> &setB)
{		// this assumes SetA and SetB are in increasing ordered
	size_t iA = 0, iB = 0, sizeA = setA.size(), sizeB = setB.size();
	vector<Terminal *> result;
	result.reserve(sizeA + sizeB);	// reserves the maximum possible size for the union (no joint elements)
	while (true)
	{
		if (iA == sizeA)	// ran out of setA elements. Just fill with rest of set B
		{
			while (iB < sizeB)
				result.push_back(setB[iB++]);
			return result;
		}
		if (iB == sizeB)	// ran out of setB elements. Just fill with rest of set A
		{
			while (iA < sizeA)
				result.push_back(setA[iA++]);
			return result;
		}

		if (setA[iA] < setB[iB])	// always add smallest element (from A or B)
			result.push_back(setA[iA++]);
		else if (setB[iB] < setA[iA])
			result.push_back(setB[iB++]);
		else
			result.push_back(setA[++iB, iA++]);	// if there is an equal element, just push back one (comma operator not overloaded)
	}
}
void FCopy(ostream &os, istream &is)
{
	char c;
	while (is.get(c), !is.eof())
		os.put(c);
}

Symbol::~Symbol() {}

void NFA::AddReductions(size_t nfaState, size_t dfaState, std::ostream &out)
{
	states[nfaState]->AddReductions(dfaState, out); // checks to see if the nfaState (in the dfaState subset) is a reduce state, and if so adds adds a reduction transition on the appropriate terminals
}
vector<bool> &NFA::Closure(vector<bool> &subset) const
{	// subset itself is modified to become the closure of the subset which was passed in
	vector<bool> checked(subset.size(), false);	// checked is used by closureRecursion to figure out which states it has already inspected. Initially no states have been inspected
	for (size_t i = 0; i < subset.size(); i++)	// loops through all the states in subset
		if (subset[i])	// closure recursion is called on all the states which are already known to be part of subset
			closureRecursion(i, checked, subset);	// closure recursion will find all the states reachable from these states by epsilon transitions and mark them as a part of subset
	return subset;	// a reference to the now modified subset is returned
}
void NFA::closureRecursion(size_t current, vector<bool> &checked, vector<bool> &subset) const
{	// current -> state which has been found to be part of closure. This function finds states linked to current by epsilon transitions. checked -> set of states whose epsilon transitions have already been checked. subset -> the closure subset being constructed
	if (!checked[current])	// checks if the state has already been instpected (in which case it is already marked). This prevents endless loops if there is an epsilon loop
	{
		checked[current] = true;	// function was called with current either because it is part of the original set or there is a transition leading to it. This marks that it is part of the closure in the latter case
		subset[current] = true;		// indicates that all the epsilon transitions were inspected. Prevents infinite loops if there is an epsilon transition loop
		for (auto tran : states[current]->TransitionList(EPSILON))	// TransitionList returns a list of id's of all the states which can be reached by one epsilon transition from current
			closureRecursion(tran, checked, subset);		// closureRecursion is then called for all these transitions to mark them in the subset and check if any other states can be reach by epsilon transitions. checked and subset are passed in by reference
	}
}
void NFA::Initialize(Production &production, const NonTerminal *nonTerminal, size_t productionNum)
{
	const size_t size = production.Size();	// gets number of symbols on RHS of production
	states.reserve(size + 1);				// reserves a start state and one state for each symbol
	for (size_t i = 0; i < size; i++)
		states.emplace_back(new State);		// fills in all the new states with empty state objects
	states.emplace_back(new State(nonTerminal, productionNum));		// last element contains a link to the production which it completes (pointer to nonterminal and number of the production)
	for (size_t i = 0; i < size; i++)	// loop to link the series of states representing a production together with transitions
	{
		states[i]->AddTransition(production[i], states[i + 1].get());	// links the ith state to (i+1)th state using the ith symbol in the production as a transition
		vector<const State *> roots = production[i]->NfaRoots();	// if production[i] is a terminal, roots is empty. if production[i] is a nonTerminal, roots contains pointer to the first states of all the NFA's for productions which reduce to that nonterminal
		for (const auto &root : roots)	// In order to have a go transition on a nonterminal, the nonterminal first has to be generated using a reduce
			states[i]->AddTransition(EPSILON, root);	// Everytime there is a nonterminal in a production, the start states of the nfa's which reduce to that nonterminal must be also linked to the state containing the transition from the nonterminal using epsilon transitions
	}
}
NFA NFA::Merge(vector<vector<NFA>> &&nfas)
{	// nfas contains all the (fully linked through transitions) pieces of the final nfa. nfas[which nonterminal][which production] = nfa
	size_t size = 0;	// size will contain the total number of states which is in all the nfa pieces (and in the final nfa)
	for (auto &vector : nfas)
		for (auto &nfa : vector)
			size += nfa.states.size();
	NFA result;
	result.states.reserve(size);
	for (auto &vector : nfas)	// all the states of the seperate NFA's are moved into the final NFA state vector. Care is taken to ensure that the first state of the start symbol production (terminated -> accepting end) is the first elemnet in states
		for (auto &nfa : vector)
			result.states.insert(result.states.end(), std::make_move_iterator(nfa.states.begin()), std::make_move_iterator(nfa.states.end()));
	for (size_t i = 0; i < result.states.size(); i++)	// each of the states are given a unique number (in a consecutive range). These id's are used when the NFA is tranformed into a DFA
		result.states[i]->AddNumber(i);
	return result;	// the Final NFA is moved from the function (returned)
}
vector<bool> NFA::Move(const vector<bool> &subset, const Symbol *symbol) const
{
	vector<bool> result(states.size(), false);
	for (size_t i = 0; i < subset.size(); i++)
		if (subset[i])
			for (auto tran : states[i]->TransitionList(symbol))
				result[tran] = true;
	return Closure(result);
}
vector<size_t> NFA::ReductionStates() const
{
	vector<size_t> result;
	for (size_t i = 0; i < states.size(); i++)		// searches through all the states and adds the numbers of the states which are reduce states. By construction, these numbers match the id's of the states
		if (states[i]->Reducible())		// reducible returns true if the state is a reduce state
			result.push_back(i);
	return result;		// By construction, the state numbers in result are in assending order.
}

vector<Terminal *> Production::ComputeFirst() const
{
	vector<Terminal *> result; // the first set of a production is the union of all the first sets of the RHS symbols up to and including the first symbol which is not nullable
	for (auto symbol : symbols)
	{
		result = SetUnion(result, symbol->First());	
		if (!symbol->Nullable())
			break;
	}
	return result;
}
bool Production::ComputeNullable() const
{
	for (auto symbol : symbols)	// a production is nullable if all the symbols on the RHS are nullable
		if (!symbol->Nullable())
			return false;
	return true;
}
void Production::FollowConstraints(const NonTerminal *lhs) const
{	// evaluates the preliminary constraints on follow sets of all the nonterminals in the production. lhs points to the lhs nonterminal of the production
	for (vector<Symbol *>::const_iterator it = symbols.begin(); it != symbols.end(); it++)
		(*it)->ExtractFollowConstraints(lhs, it + 1, symbols.end());
}
const State *Production::GetRoot(const NonTerminal *nonTerminal, size_t production)
{
	if (nfa.Empty())		// checks if the NFA for this production has been created yet
		nfa.Initialize(*this, nonTerminal, production);		// if it hasn't, a new nfa is created. This NFA contains transitions to all the NFA's which reduce to nonterminals on the RHS. The nonTerminal pointer and production number are used to create a link to the production in the reduce state
	return nfa.GetRoot();	// returns a pointer to the first state in the nfa.
}
void Production::PrintClass(ostream &os, const string name, size_t num) const
{
	os << "class " << name << num << " : public " << name << "\n"
		"{\n"
		"public:\n"
		"\t" << name << num << '(';
	if (symbols.empty())
	{
		os << ") = default;\n"
			"};\n";
		return;
	}
	for (size_t i = 0;;)
	{
		os << 'p' << symbols[i]->Name() << " &&sym";
		os << ++i;
		if (i == symbols.size())
			break;
		os << ", ";
	}
	os << ") : ";
	for (size_t i = 1;; i++)
	{
		os << "symbol_" << i << "(move(sym" << i << "))";
		if (i == symbols.size())
			break;
		os << ", ";
	}
	os << " {}\n"
		"private:\n";
	for (size_t i = 0; i < symbols.size(); i++)
		os << "\tconst p" << symbols[i]->Name() << " symbol_" << i + 1 << ";\n";	// should this be a const here?
	os << "};\n";
}
void Production::PrintDescription(ostream &os) const
{
	for (size_t i = 0; i < symbols.size(); i++)
		os << symbols[i]->Name() << " ";
	os << '\n';
}
size_t Production::PrintReduce(ostream &os) const
{
	if (!symbols.empty())
	{
		os << "\t{\n";
		for (size_t i = 0; i < symbols.size(); i++)
			os << "\t\tstack.pop();\n";
		for (size_t i = symbols.size(); i-- > 0;)
			os << "\t\tp" << symbols[i]->Name() << " sym" << i + 1 << " = pCast<" << symbols[i]->Name() << ">(move(symStack.top()));\n"
			"\t\tsymStack.pop();\n";
	}
	return symbols.size();
}

void NonTerminal::AddProduction(vector<Symbol *> &&production)
{
	productions.emplace_back(move(production));	// moves the vector of pointers to the symbols on the RHS of the production into a new Production object, located in a slot of the productions vector in the object corresponding to the LHS symbol
}
void NonTerminal::ExtractFollowConstraints(const NonTerminal *lhs, vector<Symbol *>::const_iterator it, vector<Symbol *>::const_iterator end)
{	// at minimum, the first set of a nonterminal includes the union of first sets of the symbols following it in a production
	for (; it != end; it++)	// it initially points to the next symbol in a production after the nonterminal
	{
		follow = SetUnion(follow, (*it)->First());
		if (!(*it)->Nullable())
			break;
	}
	if (it == end)		// if all the symbols to the right of the nonterminal are nullable, then the follow set of the nonterminal must include the follow set of the LHS of the production
		followConstraints.push_back(lhs);	// this is indicated by adding a pointer to the LHS nonterminal in the followConstraints set
}
bool NonTerminal::FirstSetup()
{
	vector<Terminal *> result;
	for (const auto &production : productions)
		result = SetUnion(result, production.ComputeFirst()); // the first set is the union of all the first sets of the productions of the nonterminal
	if (result.size() == first.size())	// the computed first set can only get larger (it cannot lose non-terminals) so it is sufficient to compare sizes to see if the set has changed
		return true;		// return true to indicate first set has not changed
	first = move(result);	// if sizes don't match, then move the new first set into the first set field
	return false;			// return false to indicate first set has changed
}
vector<NFA> NonTerminal::GetBranches()
{
	vector<NFA> branches;
	branches.reserve(productions.size());	// creates a vector which will hold all the NFA's
	for (auto &production : productions)
		branches.push_back(production.MoveNfa());	// moves the nfa out of each production into the branches vector (which now has ownership)
	return branches;	// the vector of all the nfa's is moved out of the function (returned)
}
void NonTerminal::MergeFollowConstraints()
{
	std::sort(followConstraints.begin(), followConstraints.end());	// puts followConstraints in assending order
	followConstraints.erase(std::unique(followConstraints.begin(), followConstraints.end()), followConstraints.end());	// eliminate duplicates
}
vector<const State *> NonTerminal::NfaRoots()
{
	vector<const State *> branches;
	branches.reserve(productions.size());	// each Production for the nonterminal has its own NFA
	for (size_t i = 0; i < productions.size(); i++)		// get the NFA's for each production
		branches.push_back(productions[i].GetRoot(this, i));	// Get's a pointer to the first state of NFA for the ith production, generating it if it doesnt already exist. the this pointer and i are needed as arguments to construct a link to the production in the reducing state
	return branches;	// a vector of all the first states for the productions is returned.
}
bool NonTerminal::NullableSetup()
{
	if (nullable)		// once a nullable production is found, a nonterminal cannot become not nullable
		return true;	// return true indicating the nullable field did not change
	for (const auto &production : productions)	// check if productions are nullable. if one production is nullable, then the nonterminal is nullable
	{
		if (production.ComputeNullable())	// ComputeNullable returns true of production is nullable
		{
			nullable = true;
			return false;	// nullable field just changed from false to true, so return false (field was changed)
		}
	}
	return true;	// no nullable productions were found so the terminal is still not nullable (field unchanged)
}
void NonTerminal::PrintActions(ostream &os) const
{
	os << "\nbool " << name << "::Process(Stack &stack, SymStack &symStack, Parser::Error &err)\n"
		"{\n"
		"\tswitch (stack.top())\n"
		"\t{\n";
	vector<bool> marked(gos.size(), false);
	for (size_t i = 0; i < gos.size(); i++)
	{
		if (!gos[i] || marked[i])
			continue;
		for (size_t j = i; j < gos.size(); j++)
		{
			if (!marked[j] && (gos[i] == gos[j]))
			{
				marked[j] = true;
				os << "\tcase " << j << ":\n";
			}
		}
		os << "\t\tstack.push(" << gos[i] - 1 << ");\n"
			"\t\tbreak;\n";
	}
	os << "\tdefault:\n"
		"\t\terr = { \"" << name << "::Process\", \"Syntax Error!\" };\n"
		"\t\treturn false;\n"
		"\t}\n"
		"\treturn true;\n"
		"}";
}
void NonTerminal::PrintClass(ostream &os) const
{
	os << "class " << name << " : public Symbol\n"
		"{\n"
		"public:\n"
		"\tvirtual ~" << name << "() = 0;\n"
		"\tstatic bool Process(Stack &stack, SymStack &symStack, Parser::Error &err);\n"
		"};\n" <<
		name << "::~" << name << "() = default;\n";
	for (size_t i = 0; i < productions.size(); i++)
		productions[i].PrintClass(os, name, i + 1);
}
void NonTerminal::PrintProductionDescription(ostream &os, size_t production) const
{
	os << name << " -> ";
	productions[production].PrintDescription(os);
}
void NonTerminal::PrintReduce(ostream &os, size_t production) const
{
	if (acceptor)
	{
		os << "\t\tbreak;\n";
		return;
	}
	size_t numSymbols = productions[production].PrintReduce(os);
	os << "\t\tsymStack.emplace(new " << name << production + 1 << '(';
	if (numSymbols)
	{
		for (size_t i = 1;; i++)
		{
			os << "move(sym" << i << ')';
			if (i == numSymbols)
				break;
			os << ", ";
		}
	}
	os << "));\n"
		"\t\treturn " << name << "::Process(stack, symStack, err) && Process(stack, symStack, err);\n";
	if (numSymbols)
		os << "\t}\n";
}
bool NonTerminal::SatisfyFollowConstraints()
{	// the follow set must contain the follow sets of all the nonTerminals in the followConstraints set
	vector<Terminal *> result = follow;	
	for (auto nonTerminal : followConstraints)
		result = SetUnion(result, nonTerminal->Follow());	//Takes the union of the current follow set, and the follow sets of all the constraints
	if (result.size() == follow.size())		// since elements cannot be removed from the follow set, it is sufficient to compare sizes to see if the follow set has changed
		return true;	// return true to indicate the follow set hasnt changed
	follow = move(result);	// save new follow set
	return false;		// return false to indicate the follow set has changed
}
void NonTerminal::SetupFollowConstraints() const
{	// evaluates the prelinary follow set constraints for the symbols on the lhs's of all the productions for this nonterminal
	for (auto &production : productions)
		production.FollowConstraints(this);
}

void Terminal::AddReduction(size_t from, pReduce &&reduce, std::ostream &out)
{
	if (actions[from]) {	// checks to see if there is already a shift transition on that state for this terminal. If there is, then there is a conflict for this grammer which needs to be resolved with a precedence rule
		char choice;
		std::cout << "Conflict:\n\tOn: " << name << "\n\t1 - ";
		reduce->PrintName(std::cout);
		std::cout << "\t2 - ";
		actions[from]->PrintName(std::cout);
		while (true) {
			choice = '\0';
			std::cout << "\tSelect action (1/2): ";
			std::cin >> choice;
			std::cin.clear();
			std::cin.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
			if (choice == '1')
			{
				out << choice << "\r\n";
				break;
			}
			else if (choice == '2')
			{
				out << choice << "\r\n";
				return;
			}
		}
	}
	actions[from] = move(reduce);		// The reduces object (with information about the production being reduced on) is added on the "from" state (the state on which the reduction should happen for this terminal)
}
void Terminal::AddShiftGo(size_t from, size_t to)
{
	actions[from] = pAction(new Shift(to));		// the "on" symbol was a terminal, so the transition is a shift. The entry in gos indicates that when this Terminal is read, and the current state is "from", then a transition should happen to the state "to"
}
void Terminal::PrintActions(ostream &os, bool isEnd) const
{
	os << "\nbool " << name << "::Process(Stack &stack, SymStack &symStack, Parser::Error &err)";
	if (!isEnd)
		os << " const";
	os << "\n{\n"
		"\tswitch (stack.top())\n"
		"\t{\n";
	vector<bool> marked(actions.size(), false);
	for (size_t i = 0; i < actions.size(); i++)
	{
		if (!actions[i] || marked[i])
			continue;
		for (size_t j = i; j < actions.size(); j++)
		{
			if (actions[j] && !marked[j] && (*actions[i] == *actions[j]))
			{
				marked[j] = true;
				os << "\tcase " << j << ":\n";
			}
		}
		actions[i]->PrintAction(os);
	}
	os << "\tdefault:\n"
		"\t\terr = { \"" << name << "::Process\", \"Syntax Error!\" };\n"
		"\t\treturn false;\n"
		"\t}\n"
		"\treturn true;\n"
		"}";
}

void State::AddReductions(size_t dfaState, std::ostream &out)
{	// dfaState is the state which contains this nfa state in its subset
	if (nonTerminal)	// if nonTerminal is not null, then State was initialized as a reducing state, so there should be a reduction to that nonterminal on this state
	{
		for (auto terminal : nonTerminal->Follow())		// the reduction should only happen if the current Terminal being read is in the follow set of the nonTerminal being reduced to
			terminal->AddReduction(dfaState, pReduce(new Reduce(nonTerminal, production)), out);	// Add the reduction to all the terminal symbols in Follow. Parameters nonTerminal and production are used to figure out how many symbols need to be popped off the stack and what nonTerminal symbol needs to put in its place. dfaState indicates from which dfa state this transition should happen
	}
}
vector<size_t> State::TransitionList(const Symbol *symbol) const
{
	vector<size_t> result;		// result will contain a list of the state id's to which there are transitions from the current state on the symbol (which can be epsilon)
	result.reserve(transitions.size());		// to speed up space for potentially all the states is reserves, so no recopies need to be performed. 
	for (auto transition : transitions)		// this loop searches all the transitions and records id's the one with matching symbols
		if (transition.symbol == symbol)
			result.push_back(transition.to->id);
	return result;		// the list of id's is not necessarily ordered
}

void Grammer::InitializeFirst()
{	// initially all nonTerminals are assumed to have no first elements. In each iteration, more terminals are recorded in the first sets of each nonterminal, until no further changes are made
	bool valid = false;
	while (!valid)
	{
		valid = true;
		for (auto &nonTerminal : nonTerminals)	// loop through all nonterminals, updating first sets
			if (!nonTerminal->FirstSetup())		// FirstSetup returns true if the nonterminal's first set is unchanged
				valid = false;					// when all nonTerminals' first sets stay constant through an iteration, then the algorithm is done
	}
}
void Grammer::InitializeFollow()
{
	for (const auto &nonTerminal : nonTerminals)
		nonTerminal->SetupFollowConstraints();	// sets up all the preliminary constraints bases on the forms of all the productions (accessed through the nonterminals)
	for (auto &nonTerminal : nonTerminals)
		nonTerminal->MergeFollowConstraints();	// removes duplicates from all the constraints sets
	bool valid = false;
	while (!valid)		// uses the constraints found previously to expand the follow sets each iteration until follow sets stop changing (the algorithm is complete)
	{
		valid = true;
		for (auto &nonTerminal : nonTerminals)
			if (!nonTerminal->SatisfyFollowConstraints())	// Expands the follow set of the nonTerminal using the constraints found previously. Returns true when follow set isn't changed
				valid = false;			// The algorithm finishes when all follow sets are constant for one iteration
	}
}
void Grammer::InitializeNullable()
{	// initally all nonTerminals are assumed not to be nullable. In each iteration, more nonterminals are forced to be marked nullable, until no further nonterminals can be marked
	bool valid = false;
	while (!valid)
	{
		valid = true;
		for (auto &nonTerminal : nonTerminals)	// loop through all nonterminals, updating their nullable field
			if (!nonTerminal->NullableSetup())	// NullableSetup returns true if the nonterminal's nullable field is unchanged
				valid = false;					// when all nonTerminals' nullable fields stay constant through an iteration then the algorithm is done
	}
}
Grammer Grammer::GetGrammer(std::ostream &out)
{
	Grammer grammer;		// sets up the head of the grammer: terminated -> acceptor end; acceptor -> start symbol of user grammer
	vector<vector<vector<string>>> productions; // preliminary productions array containing just string names productions[which nonTerminal][which production][which RHS symbol] = name

	while (true)	// loops to gather productions from user until a finished symbol ($) is entered
	{
		const NonTerminal *nonTerminal = nullptr;	// nonterminal on the LHS of the current production being entered
		string name;								// the name of the production
		std::cout << "Non-Terminal: ";
		std::cin >> name;
		out << name << "\r\n";
		if (name == "$")							// if user inputs finished symbol ($), exit the loop
			break;
		size_t i;
		for (i = 2; i < grammer.nonTerminals.size(); i++)	// loop through the already entered nonterminals to see if the current nonterminal is already present
		{			// skip the first two nonterminals, since they are not user nonterminals (terminated and acceptor)
			if (name == grammer.nonTerminals[i]->Name())	// if a name matches, make nonTerminal point to the matching nonterminal
			{
				nonTerminal = grammer.nonTerminals[i].get();
				break;
			}
		}
		if (!nonTerminal) // a matching nonterminal name was not found, so the current nonterminal is new
		{
			grammer.nonTerminals.emplace_back(new NonTerminal(move(name)));		// create a new nonterminal with the current name and add it to the back of nonTerminals
			nonTerminal = grammer.nonTerminals.back().get();
			productions.emplace_back();		// create a new entry in productions, for the newly added nonterminal. The index in productions is two smaller than the index in nonTerminals
		}
		std::cout << "\t-> ";
		productions[i - 2].emplace_back();	// add a slot for a new production for the current nonterminal
		std::cin.ignore(std::numeric_limits<std::streamsize>::max(), '\n');		// clear the input buffer
		std::getline(std::cin, name);	// and get a new line representing the full rhs of the production
		out << name << "\r\n";
		string::const_iterator it = name.begin(), begin, end = name.end();
		while (true) // break up string into space separated tokens and add those to the newly created production (just strings so far)
		{
			while (it != end && std::isspace(*it))
				it++;
			if (it == end)
				break;
			begin = it;
			while (it != end && !std::isspace(*it))
				it++;
			productions[i - 2].back().push_back(string(begin, it));
		}
	}
	grammer.nonTerminals[1]->AddProduction({ grammer.nonTerminals[2].get() }); // link the automatically generated acceptor nonterminal to the start symbol of the user grammer using a production
	for (size_t i = 0; i < productions.size(); i++) // this loop turns the primitive productions array above into actual productions, now that the names of all the nonterminals are known.
	{	// i + 2 represents the nonterminal number. Once again the two automatically generated nonterminals are not counted (terminated and acceptor)
		for (auto &production : productions[i])	// loops through the productions of the (i + 2)th nonterminal
		{
			vector<Symbol *> symbols(production.size(), nullptr);	// vector containing pointers to the actual symbol objects on the RHS of the production
			for (size_t j = 0; j < production.size(); j++)	// loops through the names on the RHS of the primitive production
			{
				for (size_t k = 2; k < grammer.nonTerminals.size(); k++)		// attempts to match the name to a nonTerminal
				{
					if (production[j] == grammer.nonTerminals[k]->Name())
					{
						symbols[j] = grammer.nonTerminals[k].get();
						break;
					}
				}
				if (symbols[j])
					continue;
				for (size_t k = 1; k < grammer.terminals.size(); k++)			// otherwise, attempts to match the name to an already encountered terminal
				{
					if (production[j] == grammer.terminals[k]->Name())
					{
						symbols[j] = grammer.terminals[k].get();
						break;
					}
				}
				if (symbols[j])
					continue;
				grammer.terminals.emplace_back(new Terminal(move(production[j])));	// if the above two failed, then the symbol is a new terminal. A new terminal object is created
				symbols[j] = grammer.terminals.back().get();
			}
			grammer.nonTerminals[i + 2]->AddProduction(move(symbols));	// once all the names are matched to symbol object, a true production is created using the symbol pointers
		}
	}
	return grammer;		// returns the now completed grammer
}
void Grammer::GenerateDfa(std::ostream &out)
{
	nonTerminals[0]->StartNfa();  // starts the recursive process of generating the individual production NFA's. The single production for the terminated nonterminal is used to generate the first NFA which recursively links to all other NFA's
	vector<vector<NFA>> nfas;	// All the NFA's for all the productions will be moved here before merging. nfas[which nonterminal][which production] = nfa
	nfas.reserve(nonTerminals.size());   // creates space for the nfa vectors for all the terminals
	for (auto &nonTerminal : nonTerminals)	// gets the nfa's for each nonterminal and adds the to the nfas vector
		nfas.push_back(nonTerminal->GetBranches());	 // GetBranches returns a vector of all the NFA's for the productions for nonTerminal. The nfa's are moved, so the returned vector now has ownership of the nfa's, not the production
	nfa = NFA::Merge(move(nfas)); // nfas, which contains disjoint pieces (but fully linked through transitions) is moved into Merge, where a single NFA containing all the linked states is created

	dfa = DFA::Generate(nfa, *this);	// The nfa is used to generate an equivalent DFA. The DFA ommits transitions on the terminated and accepting nonterminals since they are never taken during parsing. A pointer to the grammer is passed in to allow the function to access terminals and nonterminals
	DFA::Optimize(dfa);					// not implemented yet
	dfa.CreateActions(*this, out);
}
Grammer::Grammer() : dfa(nfa) // an empty NFA has already been default constructed. It is used to construct an empty dfa (the reference to a valid nfa is necessary though
{
	pNonTerminal terminated(new NonTerminal("")), acceptor(new NonTerminal("", true));  // terminated: production used to add the end of file symbol. Needed to give acceptor the correct follow set. acceptor only has end in its follow set, so a reduction on acceptor means the input is accepted.
	pTerminal end(new Terminal("End"));			// The implied end symbol of the grammer (end of file)
	terminated->AddProduction({ acceptor.get(), end.get() }); // terminated -> acceptor end. Acceptor will then have the user specified grammer attached
	nonTerminals.push_back(move(terminated));	// adds the two non terminals in order
	nonTerminals.push_back(move(acceptor));
	terminals.push_back(move(end));				// adds the end terminal
}
void Grammer::Print(ostream &os, istream &iClass, istream &iTerminals, istream &iDefinitions) const
{
	os << "#include <iostream>\n"
		"#include <istream>\n"
		"#include <memory>\n"
		"#include <stack>\n"
		"#include <string>\n"
		"#include <vector>\n\n"
		"using std::move;\n\n"
		"class Symbol;\n";
	for (size_t i = 2; i < nonTerminals.size(); i++)
		os << "class " << nonTerminals[i]->Name() << ";\n";
	os << "class Terminal;\n";
	for (size_t i = 1; i < terminals.size(); i++)
		os << "class " << terminals[i]->Name() << ";\n";
	os << "\ntypedef std::unique_ptr<Symbol> pSymbol;\n";
	for (size_t i = 2; i < nonTerminals.size(); i++)
		os << "typedef std::unique_ptr<" << nonTerminals[i]->Name() << "> p" << nonTerminals[i]->Name() << ";\n";
	os << "typedef std::unique_ptr<Terminal> pTerminal;\n";
	for (size_t i = 1; i < terminals.size(); i++)
		os << "typedef std::unique_ptr<" << terminals[i]->Name() << "> p" << terminals[i]->Name() << ";\n";
	os << "typedef std::stack<size_t, std::vector<size_t>> Stack;\n"
		"typedef std::stack<pSymbol, std::vector<pSymbol>> SymStack;\n"
		"typedef std::string::const_iterator Iterator;\n\n"
		"template<typename Dest, typename Source>\n"
		"std::unique_ptr<Dest> pCast(std::unique_ptr<Source> &&src)\n"
		"{\n"
		"\treturn std::unique_ptr<Dest>(static_cast<Dest *>(src.release()));\n"
		"}\n\n"
		"class Parser\n"
		"{\n"
		"public:\n"
		"\tParser(std::vector<pTerminal> &&in) : input(move(in)) {}\n"
		"\tbool CreateTree();\n"
		"\tp" << nonTerminals[2]->Name() << " GetTree() { return move(tree); };\n\n"
		"\tstruct Error\n"
		"\t{\n"
		"\t\tstd::string Location;\n"
		"\t\tstd::string Message;\n"
		"\t};\n"
		"\tError GetErrorReport() { return move(err); }\n"
		"private:\n"
		"\tp" << nonTerminals[2]->Name() << " tree;\n"
		"\tstd::vector<pTerminal> input;\n"
		"\tError err;\n"
		"};\n";
	FCopy(os, iClass);
	os << "\nclass Symbol\n"
		"{\n"
		"public:\n"
		"\tvirtual ~Symbol() = 0;\n"
		"};\n"
		"Symbol::~Symbol() = default;\n";
	for (size_t i = 2; i < nonTerminals.size(); i++)
		nonTerminals[i]->PrintClass(os);
	FCopy(os, iTerminals);
	/*
	os << "class Terminal : public Symbol \n"
		"{\n"
		"public:\n"
		"\tvirtual ~Terminal() = 0 {}\n"
		"\tvirtual bool Process(Stack &stack, SymStack &symStack, Parser::Error &err) const = 0;\n"
		"};\n";
	for (size_t i = 1; i < terminals.size(); i++)
		os << "class " << terminals[i]->Name() << " : public Terminal\n"
		"{\n"
		"public:\n"
		"\t~" << terminals[i]->Name() << "() = default;\n"
		"\tbool Process(Stack &stack, SymStack &symStack, Parser::Error &err) const;\n"
		"};\n";
	*/
	os << "namespace End\n"
		"{\n"
		"\tbool Process(Stack &stack, SymStack &symStack, Parser::Error &err);\n"
		"}\n\n\n\n"
		"bool Parser::CreateTree()\n"
		"{\n"
		"\tStack stack;\n"
		"\tSymStack symStack;\n"
		"\tstack.push(0);\n"
		"\tfor (auto &symbol : input)\n"
		"\t{\n"
		"\t\tif (!symbol->Process(stack, symStack, err))\n"
		"\t\t\treturn false;\n"
		"\t\tsymStack.push(move(symbol));\n"
		"\t}\n"
		"\tif (!End::Process(stack, symStack, err))\n"
		"\t\treturn false;\n"
		"\ttree = pCast<" << nonTerminals[2]->Name() << ">(move(symStack.top()));\n"
		"\treturn true;\n"
		"}";
	for (size_t i = 2; i < nonTerminals.size(); i++)
		nonTerminals[i]->PrintActions(os);
	for (size_t i = 1; i < terminals.size(); i++)
		terminals[i]->PrintActions(os);
	terminals[0]->PrintActions(os, true);
	os << "\n\n";
	FCopy(os, iDefinitions);
}

void Grammer::DFA::CreateActions(Grammer &grammer, std::ostream &out) const
{
	for (auto &nonTerminal : grammer.nonTerminals)	// clear the go transitions of all nonTerminals
		nonTerminal->PrepareGos(states.size());		// initialize the go transition vector to have the same size as the number of states and to have no transitions listed
	for (auto &terminal : grammer.terminals)		// clear the shift/reduce transitions of all the Terminals
		terminal->PrepareActions(states.size());	// initialize the shift/reduce transition vector to have the same size as the number of states and to have no transitions listed
	for (size_t i = 0; i < transitions.size(); i++)	// loop through all the dfa states to add shift/go transitions
		for (auto &transition : transitions[i])		// get the transition table for the ith state, and loop through all the transitions
			transition.on->AddShiftGo(i, transition.to);	// the transition to whichever symbol the transition occurs on. i is used to specify the state on which the transition occurs, and transition.to is used to specify the new state after the transition
	for (size_t dfaState = 0; dfaState < states.size(); dfaState++)		// A dfa state is a reduce state for a production only if it contains an nfa state with a reduction for that production
		for (size_t nfaState = 0; nfaState < states[0].size(); nfaState++) // for each state, all the nfa states are checked in its subset
			if (states[dfaState][nfaState])	// if the dfa state contains an nfa state then
				nfa.get().AddReductions(nfaState, dfaState, out);	// AddReductions checks if nfaState is a reduce state. If so, it attempts to add reductions on dfaState for all the terminals in the Follow set of nonTerminal being reduced to, requesting precedence rules for any conflicts
}
Grammer::DFA Grammer::DFA::Generate(NFA &nfa, const Grammer &grammer)  /// note uses true state numbers (instead of state + 1)
{
	DFA dfa(nfa);
	vector<size_t> reductions = nfa.ReductionStates();	// reductions contains the id's of all the states which are reduce states
	vector<bool> stateSet(nfa.Size(), false);	// vector used to represent a subset of states. stateSet[i] = state with index i is part of the set?
	stateSet[0] = true;		// Initially start with just a set containing the start state of the NFA
	dfa.states.push_back(move(nfa.Closure(stateSet))); // the start state of the dfa is created, which is just the epsilon closure of the start state of the nfa
	dfa.transitions.emplace_back();			// creates uninitialized entry for the first state
	dfa.reductionStates.emplace_back();		// creating uninitialized entry for first dfa start state. reductions in reductionState are identified by the id of the accepting nfa state associated with the reduction
	for (size_t stateIndex = 0; stateIndex < dfa.states.size(); stateIndex++)	// loop inspects all dfa states to find transitions to other dfa states (adding them as necessary). The loop initially starts with a single state, with further states being added as the loop progresses
	{
		for (auto reduction : reductions)	// checks to see if any reducing nfa states are part of states. part of dfa state states[stateIndex]
			if (dfa.states[stateIndex][reduction])	// if a reduction is found to be contained in the dfa state, the id of the associated reducing nfa states is added to reductionStates[stateIndex]
				dfa.reductionStates[stateIndex].push_back(reduction);	
		bool terminalSet = true;	// the below loop needs to go through both terminals and nonterminals. terminalSet = true indicates terminals are currently being used
		size_t index = 0;	// the index used to reference terminals/nonterminals
		Symbol *symbol;    // symbol on which the transition is currently being found
		while (true)	// loops through all the symbols, finding the dfa states reached by transitions on these symbols and adds them to the list of dfa states
		{
			if (terminalSet)	// currently looking at terminals (nonterminals will follow once all terminals have been looked at)
			{
				if (index == grammer.terminals.size())	// all the terminals have now been looked at
				{
					index = 2;				// the grammer is accepted once a reduction on the accepting nonterminal happens, so go's on the terminated and accepting nonterminals will never happen. Transitions on these symbols must be ommitted
					terminalSet = false;	// next the loop will look through nonterminals
					continue;
				}
				symbol = grammer.terminals[index].get(); // if terminalSet = true, symbol is taken from the terminals array
			}
			else
			{
				if (index == grammer.nonTerminals.size())	// finished looking at nonterminals, so the loop can terminate
					break;
				symbol = grammer.nonTerminals[index].get();	// if terminalSet = false, symbol is taken from the nonterminals array
			}
			stateSet = nfa.Move(dfa.states[stateIndex], symbol); // the dfa state (stateSet) transitioned to on symbol is the epsilon closure of the set of nfa states reachable by a single transition on symbol from any of 
			if (isNonempty(stateSet))	// stateSet will potentially be empty, indicating there are no transitions from states on that symbol
			{
				for (size_t prevStateIndex = 0;; prevStateIndex++)	// search through the already existing dfa states to see if the new state matches any
				{
					if (prevStateIndex == dfa.states.size())	// all the old states have been checked and no matches were found so a new state needs to be added
					{
						dfa.states.push_back(move(stateSet));	// the unmatched state set is moved into states as a new state
						dfa.transitions.emplace_back();			// a new transitions entry is made for this state
						dfa.transitions[stateIndex].push_back({ symbol, prevStateIndex }); // and the transition to this state found above is recorded
						dfa.reductionStates.emplace_back();		// a new entry for reductions is also created. This is filled in when the outer most loop reaches this state
						break;
					}
					else if (stateSet == dfa.states[prevStateIndex]) // the new stateSet matches a previously added state
					{
						dfa.transitions[stateIndex].push_back({ symbol, prevStateIndex });	// add the appropriate transition to the old state
						break;
					}
				}
			}
			index++;
		}
	}
	return dfa;
}
bool Grammer::DFA::isNonempty(const vector<bool> &subset)
{
	for (auto element : subset)
		if (element == true)
			return true;
	return false;
}
Grammer::DFA Grammer::DFA::Optimize(const DFA &dfa)
{
	vector<size_t> stateSets(dfa.transitions.size());
	vector<bool> marked(dfa.transitions.size(), false);
	size_t numStates = 0;
	for (size_t i = 0; i < dfa.reductionStates.size(); i++)
	{
		if (marked[i])
			continue;
		stateSets[i] = numStates;
		for (size_t j = i + 1; j < dfa.reductionStates.size(); j++)
		{
			if (!marked[j] && dfa.reductionStates[i] == dfa.reductionStates[j])
			{
				marked[j] = true;
				stateSets[i] = numStates;
			}
		}
		numStates++;
	}

	std::cout << stateSets[0] << std::endl;
	/*
	struct transition
	{
		size_t fromOld, fromNew, to;
		bool marked;
	};
	vector<size_t> states;
	states.reserve(dfa.stateInfo.size());
	size_t numStates = 1;
	bool nonAccepting = false;
	for (auto info : dfa.stateInfo)
	{
		if (!info.accepting)
			nonAccepting = true;
		if (info.accepting > numStates)
			numStates = info.accepting;
	}
	if (nonAccepting)
		numStates++;
	size_t offset = numStates - dfa.stateInfo[0].accepting;
	for (auto info : dfa.stateInfo)
		states.push_back((info.accepting + offset) % numStates + 1);
	bool consistent = false;
	while (!consistent)
	{
		consistent = true;
		for (size_t charIndex = 1; charIndex < NFA::AlphabetSize(); charIndex++)
		{
			vector<transition> transitions;
			transitions.reserve(states.size());
			vector<size_t> currentTransVals(numStates);
			for (size_t dfaState = states.size(); dfaState-- > 0;)
			{
				size_t trans = (dfa.stateInfo[dfaState].transitions[charIndex] == 0) ?
					0 : states[dfa.stateInfo[dfaState].transitions[charIndex] - 1];
				currentTransVals[states[dfaState] - 1] = trans;
				transitions.push_back({ dfaState, states[dfaState], trans });
			}
			for (size_t i = transitions.size(); i-- > 0;)
			{
				if (transitions[i].marked == false)
				{
					if (transitions[i].to != currentTransVals[transitions[i].fromNew - 1])
					{
						consistent = false;
						states[transitions[i].fromOld] = ++numStates;
						currentTransVals[transitions[i].fromNew - 1] = transitions[i].to; //experimental: num_states
					}
					for (size_t j = i; j-- > 0;)
					{
						if ((transitions[j].marked == false) && (transitions[j].fromNew == transitions[i].fromNew) &&
							(transitions[j].to == currentTransVals[transitions[j].fromNew - 1]))
						{
							transitions[j].marked = true;
							states[transitions[j].fromOld] = states[transitions[i].fromOld];
						}
					}
				}
			}
		}
	}
	stateInfo = vector<StateInfo>(numStates);
	for (size_t i = 0; i < states.size(); i++)
	{
		stateInfo[states[i] - 1].accepting = dfa.stateInfo[i].accepting;
		for (size_t j = 1; j < NFA::AlphabetSize(); j++)
			stateInfo[states[i] - 1].transitions[j] = (dfa.stateInfo[i].transitions[j] == 0) ? 0 : states[dfa.stateInfo[i].transitions[j] - 1];
	}
	*/
	return DFA(dfa.nfa);
}

Action::~Action() {}
void Shift::PrintAction(ostream &os) const
{
	os << "\t\tstack.push(" << to << ");\n"
		"\t\tbreak;\n";
}
void Reduce::PrintAction(ostream &os) const
{
	nonTerminal->PrintReduce(os, production);
}
void Reduce::PrintName(ostream &os) const 
{
	os << "Reduce: ";
	nonTerminal->PrintProductionDescription(os, production);
}