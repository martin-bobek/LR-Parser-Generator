#include <algorithm>
#include <fstream>
#include <functional>
#include <iostream>
#include <memory>
#include <string>
#include <vector>
#include <cctype>

#define EPSILON		nullptr

// try to restrict access as much as possible
// try to remove as many move's as possible (remember to write both a default move constructor and assignment in all relevant classes!).
// add as many noexcepts as possible

using std::move;
using std::ostream;
using std::string;
using std::vector;

class Production;
class Terminal;
class NonTerminal;
class State;
class NFA;
class Action;
class Shift;
class Reduce;
typedef std::unique_ptr<Action> pAction;
typedef std::unique_ptr<Reduce> pReduce;
typedef std::unique_ptr<State> pState;
typedef std::unique_ptr<NonTerminal> pNonTerminal;
typedef std::unique_ptr<Terminal> pTerminal;
typedef std::reference_wrapper<NFA> rNFA;

vector<Terminal *> SetUnion(const vector<Terminal *> &setA, const vector<Terminal *> &setB);

class Symbol
{
public:
	virtual ~Symbol() = 0 {}
	virtual bool Nullable() const = 0;
	virtual vector<Terminal *> First() = 0;
	virtual void ExtractFollowConstraints(const NonTerminal *lhs, vector<Symbol *>::const_iterator it, vector<Symbol *>::const_iterator end) = 0;
	virtual vector<const State *> NfaRoots() = 0;
	string Name() const { return name; }

	virtual void AddShiftGo(size_t from, size_t to) = 0;
protected:
	Symbol(string &&name) : name(move(name)) {}
	string name;										// should this be private
};
class NFA
{
public:
	NFA() = default;
	void Initialize(Production &production, const NonTerminal *nonTerminal = nullptr, size_t numProduction = 0);
	NFA(const NFA &) = delete;
	NFA(NFA &&rhs) = default;
	NFA &operator=(const NFA &) = delete;
	NFA &operator=(NFA &&rhs) = default;
	bool Empty() const { return states.empty(); }
	const State *GetRoot() { return states[0].get(); }
	static NFA Merge(vector<vector<NFA>> &&nfas);
	void AddReductions(size_t nfaState, size_t dfaState);
	size_t Size() const { return states.size(); }
	vector<bool> &Closure(vector<bool> &subset) const;
	vector<bool> Move(const vector<bool> &subset, const Symbol *symbol) const;
	vector<size_t> ReductionStates() const;
private:
	void closureRecursion(size_t current, vector<bool> &checked, vector<bool> &subset) const;
	vector<pState> states;
};
class Production
{
public:
	bool ComputeNullable() const;
	vector<Terminal *> ComputeFirst() const;
	void FollowConstraints(const NonTerminal *) const;

	void StartNfa() { nfa.Initialize(*this); }
	NFA MoveNfa() { return move(nfa); }
	const State *GetRoot(const NonTerminal *nonTerminal, size_t production);
	size_t Size() const { return symbols.size(); }
	Symbol *operator[](size_t index) const { return symbols[index]; }

	void PrintClass(ostream &os, const string name, size_t num) const;
private:
	vector<Symbol *> symbols;
	NFA nfa;
	bool nfa_init = false;
public:
	Production(vector<Symbol *> &&symbols) : symbols(move(symbols)) {}
	size_t PrintReduce(ostream &os) const;
};
class NonTerminal : public Symbol
{
public:
	NonTerminal(string &&name, bool acceptor = false) : Symbol(move(name)), acceptor(acceptor) {}
	bool Nullable() const { return nullable; };
	bool NullableSetup();
	vector<Terminal *> First() { return first; }
	bool FirstSetup();
	vector<Terminal *> Follow() const { return follow; }
	void ExtractFollowConstraints(const NonTerminal *lhs, vector<Symbol *>::const_iterator it, vector<Symbol *>::const_iterator end);
	void SetupFollowConstraints() const;
	void MergeFollowConstraints();
	bool SatisfyFollowConstraints();
	vector<const State *> NfaRoots();
	vector<NFA> GetBranches();
	void StartNfa() { productions[0].StartNfa(); }

	void PrintClass(ostream &os) const;
private:
	bool nullable = false;
	vector<Terminal *> first;
	vector<Terminal *> follow;
	vector <const NonTerminal *> followConstraints;
	vector<Production> productions;
	vector<size_t> gos;
	bool acceptor;
public:			
	void AddProduction(vector<Symbol *> &&production);
	void NonTerminal::PrintActions(ostream &os) const;
	void PrintReduce(ostream &os, size_t production) const;
	void PrepareGos(size_t numStates) { gos.resize(numStates); }
	void AddShiftGo(size_t from, size_t to) { gos[from] = to + 1; }
};
class Terminal : public Symbol
{
public:
	Terminal(string &&name) : Symbol(move(name)) {}
	bool Nullable() const { return false; }
	vector<Terminal *> First() { return{ this }; }
	void ExtractFollowConstraints(const NonTerminal *, vector<Symbol *>::const_iterator, vector<Symbol *>::const_iterator) {}
	vector<const State *> NfaRoots() { return vector<const State *>(); }
	void PrintActions(ostream &os) const;

	void PrepareActions(size_t numStates) { actions.resize(numStates); }
	void AddShiftGo(size_t from, size_t to);
	void AddReductions(size_t from, pReduce &&reduce);
private:
	vector<pAction> actions;
};
class State
{
public:
	State(const NonTerminal *nonTerminal = nullptr, size_t production = 0) : nonTerminal(nonTerminal), production(production) {}
	void AddTransition(const Symbol *symbol, const State *to) { transitions.push_back({ symbol, to }); }
	void AddReductions(size_t state);
	void AddNumber(size_t numState) { num = numState; };
	std::vector<size_t> TransitionList(const Symbol *symbol) const;
	bool Reducible() const { return nonTerminal ? true : false; }
private:
	struct Transition
	{
		const Symbol * symbol;
		const State * to;
	};
	vector<Transition> transitions;
	const NonTerminal *nonTerminal;
	size_t production;
	//pReduce accepting;
	size_t num;
};
class Grammer
{
public:
	static Grammer GetGrammer();
	void InitializeNullable();
	void InitializeFirst();
	void InitializeFollow();
	void GenerateDfa();
	void AddNonTerminal(pNonTerminal &&nonTerminal) { nonTerminals.push_back(move(nonTerminal)); }
	void AddTerminal(pTerminal &&terminal) { terminals.push_back(move(terminal)); }

	void Print(ostream &os) const;
private:
	Grammer();
	class DFA
	{
	public:
		DFA(NFA &nfa) : nfa(nfa) {}
		DFA(DFA &&) = default;
		DFA &operator=(DFA &&) = default;
		static DFA Generate(NFA &nfa, const Grammer &grammer);
		static DFA Optimize(const DFA &dfa);
		void CreateActions(Grammer &grammer) const;
	private:
		static bool isNonempty(const vector<bool> &subset);
		vector<vector<bool>> states;
		struct Transition
		{
			Symbol *on;
			size_t to;
		};
		vector<vector<Transition>> transitions;
		vector<vector<size_t>> reductionStates;
		rNFA nfa;
	};
	NFA nfa;
	DFA dfa;

	vector<pNonTerminal> nonTerminals;
	vector<pTerminal> terminals;
};
class Action
{
public:
	virtual ~Action() = 0 {}
	virtual void PrintAction(ostream &os) const = 0;
	virtual bool operator==(const Action &) const = 0;
	virtual bool operator==(const Shift &) const = 0;
	virtual bool operator==(const Reduce &) const = 0;
};
class Shift : public Action
{
public:
	Shift(size_t to) : to(to) {}
	void PrintAction(ostream &os) const;
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
	bool operator==(const Action &rhs) const { return rhs == *this; }
	bool operator==(const Shift &) const { return false; }
	bool operator==(const Reduce &rhs) const { return (nonTerminal == rhs.nonTerminal) && (production == rhs.production); }
private:
	const NonTerminal *nonTerminal;
	size_t production;
};


int main(int argc, char *argv[])
{
	if (argc != 2)
	{
		std::cerr << "Improper number of arguments entered!" << std::endl;
		return 1;
	}
/*
	Terminal a('a'), b('b');
	NonTerminal *N = new NonTerminal(),
				*A = new NonTerminal(),
				*B = new NonTerminal(),
				*C = new NonTerminal();
	N->AddProduction(Production({ A, B }));
	N->AddProduction(Production({ B, A }));
	A->AddProduction(Production({ &a }));
	A->AddProduction(Production({ C, A, C }));
	B->AddProduction(Production({ &b }));
	B->AddProduction(Production({ C, B, C }));
	C->AddProduction(Production({ &a }));
	C->AddProduction(Production({ &b }));
	Grammer grammer = Grammer(pNonTerminal(N));
	grammer.AddNonTerminal(pNonTerminal(A));
	grammer.AddNonTerminal(pNonTerminal(B));
	grammer.AddNonTerminal(pNonTerminal(C));
*/
	Grammer grammer = Grammer::GetGrammer();
	grammer.InitializeNullable();
	grammer.InitializeFirst();
	grammer.InitializeFollow();
	grammer.GenerateDfa();
	
	try
	{
		std::ofstream os(argv[1]);
		if (os.fail())
		{
			std::cerr << "Failed to open file!" << std::endl;
			return 1;
		}
		grammer.Print(os);
	}
	catch (char *msg)
	{
		std::cerr << msg << std::endl;
	}
	system("pause");
}

vector<Terminal *> SetUnion(const vector<Terminal *> &setA, const vector<Terminal *> &setB)
{
	size_t iA = 0, iB = 0, sizeA = setA.size(), sizeB = setB.size();
	vector<Terminal *> result;
	result.reserve(sizeA + sizeB);
	while (true)
	{
		if (iA == sizeA)
		{
			while (iB < sizeB)
				result.push_back(setB[iB++]);
			return result;
		}
		if (iB == sizeB)
		{
			while (iA < sizeA)
				result.push_back(setA[iA++]);
			return result;
		}

		if (setA[iA] < setB[iB])
			result.push_back(setA[iA++]);
		else if (setB[iB] < setA[iA])
			result.push_back(setB[iB++]);
		else
			result.push_back(setA[++iB, iA++]);
	}
}

void NFA::AddReductions(size_t nfaState, size_t dfaState)
{
	states[nfaState]->AddReductions(dfaState);
}
vector<bool> &NFA::Closure(vector<bool> &subset) const
{
	vector<bool> checked(subset.size(), false);
	for (size_t i = 0; i < subset.size(); i++)
		if (subset[i])
			closureRecursion(i, checked, subset);
	return subset;
}
void NFA::closureRecursion(size_t current, vector<bool> &checked, vector<bool> &subset) const
{
	if (!checked[current])
	{
		checked[current] = true;
		subset[current] = true;
		for (auto tran : states[current]->TransitionList(EPSILON))
			closureRecursion(tran, checked, subset);
	}
}
void NFA::Initialize(Production &production, const NonTerminal *nonTerminal, size_t productionNum)
{
	const size_t size = production.Size();
	states.reserve(size + 1);
	for (size_t i = 0; i < size; i++)
		states.push_back(pState(new State));
	states.push_back(pState(new State(nonTerminal, productionNum)));
	for (size_t i = 0; i < size; i++)
	{
		states[i]->AddTransition(production[i], states[i + 1].get());
		vector<const State *> roots = production[i]->NfaRoots();
		for (const auto &root : roots)
			states[i]->AddTransition(EPSILON, root);
	}
}
NFA NFA::Merge(vector<vector<NFA>> &&nfas)
{
	size_t size = 0;
	for (auto &vector : nfas)
		for (auto &nfa : vector)
			size += nfa.states.size();
	NFA result;
	result.states.reserve(size);
	for (auto &vector : nfas)
		for (auto &nfa : vector)
			result.states.insert(result.states.end(), std::make_move_iterator(nfa.states.begin()), std::make_move_iterator(nfa.states.end()));
	for (size_t i = 0; i < result.states.size(); i++)
		result.states[i]->AddNumber(i);																			// check if this is right
	return result;
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
	for (size_t i = 0; i < states.size(); i++)
		if (states[i]->Reducible())
			result.push_back(i);
	return result;
}

vector<Terminal *> Production::ComputeFirst() const
{
	vector<Terminal *> result;
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
	for (auto symbol : symbols)
		if (!symbol->Nullable())
			return false;
	return true;
}
void Production::FollowConstraints(const NonTerminal *lhs) const
{
	for (vector<Symbol *>::const_iterator it = symbols.begin(); it != symbols.end(); it++)
		(*it)->ExtractFollowConstraints(lhs, it + 1, symbols.end());
}
const State *Production::GetRoot(const NonTerminal *nonTerminal, size_t production)
{
	if (nfa.Empty())
		nfa.Initialize(*this, nonTerminal, production);
	return nfa.GetRoot();
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
			"\t~" << name << num << "() = default;\n"
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
		"\t~" << name << num << "() = default;\n"
		"private:\n";
	for (size_t i = 0; i < symbols.size(); i++)
		os << "\tconst p" << symbols[i]->Name() << " symbol_" << i + 1 << ";\n";	// should this be a const here?
	os << "};\n";
}
size_t Production::PrintReduce(ostream &os) const
{
	if (!symbols.empty())
	{
		os << "\t{\n";
		for (auto &symbol : symbols)
			os << "\t\tstack.pop()\n";
		for (size_t i = symbols.size(); i-- > 0;)
			os << "\t\tp" << symbols[i]->Name() << " sym" << i + 1 << " = pCast<" << symbols[i]->Name() << ">(move(symStack.top()));\n"
			"\t\tsymStack.pop();\n";
	}
	return symbols.size();
}

void NonTerminal::AddProduction(vector<Symbol *> &&production)
{
	productions.emplace_back(move(production));
}
void NonTerminal::ExtractFollowConstraints(const NonTerminal *lhs, vector<Symbol *>::const_iterator it, vector<Symbol *>::const_iterator end)
{
	for (; it != end; it++)
	{
		follow = SetUnion(follow, (*it)->First());
		if (!(*it)->Nullable())
			break;
	}
	if (it == end)
		followConstraints.push_back(lhs);
}
bool NonTerminal::FirstSetup()
{
	vector<Terminal *> result;
	for (const auto &production : productions)
		result = SetUnion(result, production.ComputeFirst());
	if (result.size() == first.size())
		return true;
	first = move(result);
	return false;
}
vector<NFA> NonTerminal::GetBranches()
{
	vector<NFA> branches;
	branches.reserve(productions.size());
	for (auto &production : productions)
		branches.push_back(production.MoveNfa());
	return branches;
}
void NonTerminal::MergeFollowConstraints()
{
	std::sort(followConstraints.begin(), followConstraints.end());
	followConstraints.erase(std::unique(followConstraints.begin(), followConstraints.end()), followConstraints.end());
}
vector<const State *> NonTerminal::NfaRoots()
{
	vector<const State *> branches;
	branches.reserve(productions.size());
	for (size_t i = 0; i < productions.size(); i++)
		branches.push_back(productions[i].GetRoot(this, i));
	return branches;
}
bool NonTerminal::NullableSetup()
{
	if (nullable)
		return true;
	for (const auto &production : productions)
	{
		if (production.ComputeNullable())
		{
			nullable = true;
			return false;
		}
	}
	return true;
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
		"\tvirtual ~" << name << "() = 0 {}\n"
		"\tstatic bool Process(Stack &stack, SymStack &symStack, Parser::Error &err);\n"
		"};\n";
	for (size_t i = 0; i < productions.size(); i++)
		productions[i].PrintClass(os, name, i + 1);
}
void NonTerminal::PrintReduce(ostream &os, size_t production) const
{
	if (acceptor)
	{
		os << "\t\treturn true;\n";
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
{
	vector<Terminal *> result = follow;
	for (auto nonTerminal : followConstraints)
		result = SetUnion(result, nonTerminal->Follow());
	if (result.size() == follow.size())
		return true;
	follow = move(result);
	return false;
}
void NonTerminal::SetupFollowConstraints() const
{
	for (auto &production : productions)
		production.FollowConstraints(this);
}

void Terminal::AddReductions(size_t from, pReduce &&reduce)
{
	if (!actions[from])
		std::cerr << "Invalid Grammer!" << std::endl;
	actions[from] = move(reduce);
}
void Terminal::AddShiftGo(size_t from, size_t to)
{
	actions[from] = pAction(new Shift(to));
}
void Terminal::PrintActions(ostream &os) const
{
	os << "\nbool " << name << "::Process(Stack &stack, SymStack &symStack, Parser::Error &err) const\n"
		"{\n"
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

void State::AddReductions(size_t state)
{
	if (nonTerminal)
	{
		for (auto terminal : nonTerminal->Follow())
			terminal->AddReductions(state, pReduce(new Reduce(nonTerminal, production)));
	}
}
std::vector<size_t> State::TransitionList(const Symbol *symbol) const
{
	vector<size_t> result;
	result.reserve(transitions.size());
	for (auto transition : transitions)
		if (transition.symbol == symbol)
			result.push_back(transition.to->num);
	return result;
}

void Grammer::InitializeFirst()
{
	bool valid = false;
	while (!valid)
	{
		valid = true;
		for (auto &nonTerminal : nonTerminals)
			if (!nonTerminal->FirstSetup())
				valid = false;
	}
}
void Grammer::InitializeFollow()
{
	for (const auto &nonTerminal : nonTerminals)
		nonTerminal->SetupFollowConstraints();
	for (auto &nonTerminal : nonTerminals)
		nonTerminal->MergeFollowConstraints();
	bool valid = false;
	while (!valid)
	{
		valid = true;
		for (auto &nonTerminal : nonTerminals)
			if (!nonTerminal->SatisfyFollowConstraints())
				valid = false;
	}
}
void Grammer::InitializeNullable()
{
	bool valid = false;
	while (!valid)
	{
		valid = true;
		for (auto &nonTerminal : nonTerminals)
			if (!nonTerminal->NullableSetup())
				valid = false;
	}
}
Grammer Grammer::GetGrammer()
{
	Grammer grammer;
	vector<vector<vector<string>>> productions;

	while (true)
	{
		const NonTerminal *nonTerminal = nullptr;
		std::string name;
		std::cout << "Non-Terminal: ";
		std::cin >> name;
		if (name == "$")
			break;
		//for (const auto &nonTerm : grammer.nonTerminals)
		size_t i;
		for (i = 2; i < grammer.nonTerminals.size(); i++)
		{
			if (name == grammer.nonTerminals[i]->Name())
			{
				nonTerminal = grammer.nonTerminals[i].get();
				break;
			}
		}
		if (!nonTerminal)
		{
			grammer.nonTerminals.emplace_back(new NonTerminal(move(name)));
			nonTerminal = grammer.nonTerminals.back().get();
			productions.emplace_back();
		}
		std::cout << "\t-> ";
		productions[i - 2].emplace_back();
		std::cin.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
		std::getline(std::cin, name);
		string::const_iterator it = name.begin(), begin, end = name.end();
		while (true)
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
	grammer.nonTerminals[1]->AddProduction({ grammer.nonTerminals[2].get() }); 
	for (size_t i = 0; i < productions.size(); i++)
	{
		for (auto &production : productions[i])
		{
			std::vector<Symbol *> symbols(production.size(), nullptr);
			for (size_t j = 0; j < production.size(); j++)
			{
				for (size_t k = 2; k < grammer.nonTerminals.size(); k++)
				{
					if (production[j] == grammer.nonTerminals[k]->Name())
					{
						symbols[j] = grammer.nonTerminals[k].get();
						break;
					}
				}
				if (symbols[j])
					continue;
				for (size_t k = 1; k < grammer.terminals.size(); k++)
				{
					if (production[j] == grammer.terminals[k]->Name())
					{
						symbols[j] = grammer.terminals[k].get();
						break;
					}
				}
				if (symbols[j])
					continue;
				grammer.terminals.emplace_back(new Terminal(move(production[j])));
				symbols[j] = grammer.terminals.back().get();
			}
			grammer.nonTerminals[i + 2]->AddProduction(move(symbols));
		}
	}
	return grammer;
}
void Grammer::GenerateDfa()
{
	nonTerminals[0]->StartNfa();
	vector<vector<NFA>> nfas;
	nfas.reserve(nonTerminals.size());
	for (auto &nonTerminal : nonTerminals)
		nfas.push_back(nonTerminal->GetBranches());
	nfa = NFA::Merge(move(nfas));

	dfa = DFA::Generate(nfa, *this);
	DFA::Optimize(dfa);
	dfa.CreateActions(*this);
}
Grammer::Grammer() : dfa(nfa)
{
	pNonTerminal terminated(new NonTerminal("")), acceptor(new NonTerminal("", true));
	pTerminal end(new Terminal("End"));
	terminated->AddProduction({ acceptor.get(), end.get() });
	nonTerminals.push_back(move(terminated));
	nonTerminals.push_back(move(acceptor));
	terminals.push_back(move(end));
}
void Grammer::Print(ostream &os) const
{
	os << "#include <iostream>\n"
		"#include <memory>\n"
		"#include <stack>\n"
		"#include <string>\n"
		"#include <vector>\n\n"
		"using move;\n\n"
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
	os << "typedef std::stack<size_t, vector<size_t>> Stack;\n"
		"typedef std::stack<pSymbol, vector<pSymbol>> SymStack;\n\n"
		"template<typename Dest, typename Source>\n"
		"std::unique_ptr<Dest> pCast(std::unique_ptr<Source> &&src)\n"
		"{\n"
		"\treturn std::unique_ptr<Dest>(static_cast<Dest *>(src.release()));\n"
		"}\n\n"
		"class Parser\n"
		"{\n"
		"public:\n"
		"\tParser(vector<pTerminal> &&in) : input(move(in)) {}\n"
		"\tbool CreateTree();\n"
		"\tpT GetTree() { return move(tree); };\n\n"
		"\tstruct Error\n"
		"\t{\n"
		"\t\tstring Location;\n"
		"\t\tstring Message;\n"
		"\t};\n"
		"\tError GetErrorReport() { return move(err); }\n"
		"private:\n"
		"\tpT tree;\n"
		"\tvector<pTerminal> input;\n"
		"\tError err;\n"
		"};\n\n"
		"class Symbol\n"
		"{\n"
		"public:\n"
		"\tvirtual ~Symbol() = 0 {}\n"
		"};\n";
	for (size_t i = 2; i < nonTerminals.size(); i++)
		nonTerminals[i]->PrintClass(os);
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
	os << "namespace End \n"
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
		"\ttree = pCast<T>(move(symStack.top()));\n"
		"\treturn true;\n"
		"}";
	for (size_t i = 2; i < nonTerminals.size(); i++)
		nonTerminals[i]->PrintActions(os);
	for (size_t i = 1; i < terminals.size(); i++)
		terminals[i]->PrintActions(os);
	terminals[0]->PrintActions(os);
}

void Grammer::DFA::CreateActions(Grammer &grammer) const // shuld this be a reference member
{
	for (auto &nonTerminal : grammer.nonTerminals)
		nonTerminal->PrepareGos(transitions.size());
	for (auto &terminal : grammer.terminals)
		terminal->PrepareActions(transitions.size());
	for (size_t i = 0; i < transitions.size(); i++)
		for (auto &transition : transitions[i])
			transition.on->AddShiftGo(i, transition.to);
	for (size_t i = 0; i < states.size(); i++)
		for (size_t j = 0; j < states[0].size(); j++)
			if (states[i][j])
				nfa.get().AddReductions(j, i);
}
Grammer::DFA Grammer::DFA::Generate(NFA &nfa, const Grammer &grammer)  /// note uses true state numbers (instead of state + 1)
{
	DFA dfa(nfa);
	vector<size_t> reductions = nfa.ReductionStates();
	vector<bool> stateSet(nfa.Size(), false);
	stateSet[0] = true;
	dfa.states.push_back(move(nfa.Closure(stateSet)));
	dfa.transitions.emplace_back();
	dfa.reductionStates.emplace_back();
	for (size_t stateIndex = 0; stateIndex < dfa.states.size(); stateIndex++)
	{
		for (auto reduction : reductions)
			if (dfa.states[stateIndex][reduction])
				dfa.reductionStates[stateIndex].push_back(reduction);
		bool terminalSet = true;
		size_t index = 0;
		Symbol *symbol;
		while (true)
		{
			if (terminalSet)
			{
				if (index == grammer.terminals.size())
				{
					index = 2;
					terminalSet = false;
					continue;
				}
				symbol = grammer.terminals[index].get();
			}
			else
			{
				if (index == grammer.nonTerminals.size())
					break;
				symbol = grammer.nonTerminals[index].get();
			}
			stateSet = nfa.Move(dfa.states[stateIndex], symbol); /// the states of course can overlap so dfa.states is not a valid representation.
			if (isNonempty(stateSet))
			{
				for (size_t prevStateIndex = 0;; prevStateIndex++)
				{
					if (prevStateIndex == dfa.states.size())
					{
						dfa.states.push_back(move(stateSet));
						dfa.transitions.emplace_back();
						dfa.transitions[stateIndex].push_back({ symbol, prevStateIndex });
						dfa.reductionStates.emplace_back();
						break;
					}
					else if (stateSet == dfa.states[prevStateIndex])
					{
						dfa.transitions[stateIndex].push_back({ symbol, prevStateIndex });
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

	std::cout << stateSets[0];
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

void Shift::PrintAction(ostream &os) const
{
	os << "\t\tstack.push(" << to << ");\n"
		"\t\tbreak;\n";
}

void Reduce::PrintAction(ostream &os) const
{
	nonTerminal->PrintReduce(os, production);
}
