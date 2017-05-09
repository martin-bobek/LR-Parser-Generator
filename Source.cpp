#include <algorithm>
#include <fstream>
#include <iostream>
#include <memory>
#include <vector>

#define GRAMMER		2
#define EPSILON		'\0'

// try to restrict access as much as possible
// try to remove as many std::move's as possible (remember to write both a default move constructor and assignment in all relevant classes!).

class Production;
class Terminal;
class NonTerminal;
class State;
class NFA;

typedef std::unique_ptr<State> pState;
typedef std::unique_ptr<NonTerminal> pNonTerminal;

class NFA
{
public:
	NFA() = default;
	NFA(Production &production);
	NFA(const NFA &) = delete;
	NFA(NFA &&rhs) = default;
	NFA &operator=(const NFA &) = delete;
	NFA &operator=(NFA &&rhs) = default;
	bool Empty() const { return states.empty(); }
	const State *GetRoot() { return states[0].get(); }
	static NFA Merge(std::vector<std::vector<NFA>> &&nfas);
private:

	std::vector<pState> states;
};

std::vector<const Terminal *> SetUnion(const std::vector<const Terminal *> &setA, const std::vector<const Terminal *> &setB);

class Symbol
{
public:
	virtual bool Nullable() const = 0;
	virtual std::vector<const Terminal *> First() const = 0;
	virtual void ExtractFollowConstraints(const NonTerminal *lhs, std::vector<Symbol *>::const_iterator it, std::vector<Symbol *>::const_iterator end) = 0;
	virtual std::vector<const State *> NfaRoots() = 0;
};
class NonTerminal : public Symbol
{
public:
	bool Nullable() const { return nullable; };
	bool NullableSetup();
	std::vector<const Terminal *> First() const { return first; }
	bool FirstSetup();
	std::vector<const Terminal *> Follow() const { return follow; }
	void ExtractFollowConstraints(const NonTerminal *lhs, std::vector<Symbol *>::const_iterator it, std::vector<Symbol *>::const_iterator end);
	void SetupFollowConstraints() const;
	void MergeFollowConstraints();
	bool SatisfyFollowConstraints();
	std::vector<const State *> NfaRoots();
	std::vector<NFA> GetBranches();
private:
	bool nullable = false;
	std::vector<const Terminal *> first;
	std::vector<const Terminal *> follow;
	std::vector <const NonTerminal *> followConstraints;
	std::vector<Production> productions;

public:			
	void AddProduction(Production &&production);
};
class Terminal : public Symbol
{
public:
	Terminal(char c) : c(c) {}
	bool Nullable() const { return false; }
	std::vector<const Terminal *> First() const { return{ this }; }
	void ExtractFollowConstraints(const NonTerminal *, std::vector<Symbol *>::const_iterator, std::vector<Symbol *>::const_iterator) {}
	std::vector<const State *> NfaRoots() { return std::vector<const State *>(); }
private:
	char c;
};
class Production
{
public:
	bool ComputeNullable() const;
	std::vector<const Terminal *> ComputeFirst() const;
	void FollowConstraints(const NonTerminal *) const;

	NFA MoveNfa() { return std::move(nfa); }
	const State *GetRoot();
	size_t Size() const { return symbols.size(); }
	Symbol *operator[](size_t index) const { return symbols[index]; }
private:
	std::vector<Symbol *> symbols;
	NFA nfa;
	bool nfa_init = false;
public:
	Production(std::vector<Symbol *> &&symbols) : symbols(std::move(symbols)) {}
};
class Grammer
{
public:
	void InitializeNullable();
	void InitializeFirst();
	void InitializeFollow();
	NFA GetNfa() const;
	Grammer(pNonTerminal &&start);
	void AddNonTerminal(pNonTerminal &&nonTerminal);
private:
	std::vector<pNonTerminal> nonTerminals;
};

class State
{
public:
	State(bool accepting = false) : accepting(accepting) {}
	void AddTransition(const Symbol *symbol, const State *to) { transitions.push_back({ symbol, to }); }
private:
	struct Transition
	{
		const Symbol * const symbol;
		const State * const to;
	};
	std::vector<Transition> transitions;
	const bool accepting;
};

std::vector<NFA> NonTerminal::GetBranches()
{
	std::vector<NFA> branches;
	branches.reserve(productions.size());
	for (auto &production : productions)
		branches.push_back(production.MoveNfa());
	return branches;
}
const State *Production::GetRoot()
{
	if (nfa.Empty())
		nfa = NFA(*this);
	return nfa.GetRoot();
}
std::vector<const State *> NonTerminal::NfaRoots()
{
	std::vector<const State *> branches;
	branches.reserve(productions.size());
	for (auto &production : productions)
		branches.push_back(production.GetRoot());
	return branches;
}
NFA NFA::Merge(std::vector<std::vector<NFA>> &&nfas)
{
	size_t size = 0;
	for (auto &vector : nfas)
		for (auto &nfa : vector)
			size += nfa.states.size();
	NFA result;
	result.states.reserve(size);
	for (auto &vector : nfas)
		for (auto &nfa : vector)
			result.states.insert(result.states.end(), std::make_move_iterator(nfa.states.begin()), std::make_move_iterator(nfa.states.begin()));
	return result;
}
NFA::NFA(Production &production)
{
	const size_t size = production.Size();
	states.reserve(size + 1);
	for (size_t i = 0; i < size; i++)
		states.push_back(pState(new State));
	states.push_back(pState(new State(true)));
	for (size_t i = 0; i < size; i++)
	{
		states[i]->AddTransition(production[i], states[i + 1].get());
		std::vector<const State *> roots = production[i]->NfaRoots();
		for (const auto &root : roots)
			states[i]->AddTransition(EPSILON, root);
	}
}
NFA Grammer::GetNfa() const
{
	std::vector<std::vector<NFA>> nfas;
	nfas.reserve(nonTerminals.size());
	for (auto &nonTerminal : nonTerminals)
		nfas.push_back(nonTerminal->GetBranches());
	return NFA::Merge(std::move(nfas));
}

int main()
{
#if (GRAMMER == 1)
	Terminal c('@'), star('*'), ors('|'), open('('), close(')');
	NonTerminal *Q = new NonTerminal(),
				*R = new NonTerminal(),
				*S = new NonTerminal(),
				*T = new NonTerminal(),
				*U = new NonTerminal(),
				*V = new NonTerminal(),
				*W = new NonTerminal();
	Q->AddProduction(Production({ S, R }));
	R->AddProduction(Production({ &ors, S, R }));
	R->AddProduction(Production({}));
	S->AddProduction(Production({ U, T }));
	T->AddProduction(Production({ U, T }));
	T->AddProduction(Production({}));
	U->AddProduction(Production({ W, V }));
	V->AddProduction(Production({ &star, V }));
	V->AddProduction(Production({}));
	W->AddProduction(Production({ &c }));
	W->AddProduction(Production({ &open, Q, &close }));
	Grammer grammer = Grammer(std::unique_ptr<NonTerminal>(Q));
	grammer.AddNonTerminal(std::unique_ptr<NonTerminal>(R));
	grammer.AddNonTerminal(std::unique_ptr<NonTerminal>(S));
	grammer.AddNonTerminal(std::unique_ptr<NonTerminal>(T));
	grammer.AddNonTerminal(std::unique_ptr<NonTerminal>(U));
	grammer.AddNonTerminal(std::unique_ptr<NonTerminal>(V));
	grammer.AddNonTerminal(std::unique_ptr<NonTerminal>(W));
#endif
#if (GRAMMER == 2)
	Terminal a('a'), b('b'), c('c');
	NonTerminal *T = new NonTerminal(), *R = new NonTerminal();
	T->AddProduction(Production({ R }));
	T->AddProduction(Production({ &a, T, &c }));
	R->AddProduction(Production({}));
	R->AddProduction(Production({ R, &b, R }));
	Grammer grammer = Grammer(std::unique_ptr<NonTerminal>(T));
	grammer.AddNonTerminal(std::unique_ptr<NonTerminal>(R));
#endif
#if (GRAMMER == 3)
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
	Grammer grammer = Grammer(std::unique_ptr<NonTerminal>(N));
	grammer.AddNonTerminal(std::unique_ptr<NonTerminal>(A));
	grammer.AddNonTerminal(std::unique_ptr<NonTerminal>(B));
	grammer.AddNonTerminal(std::unique_ptr<NonTerminal>(C));
#endif
	grammer.InitializeNullable();
	grammer.InitializeFirst();
	grammer.InitializeFollow();
	NFA nfa = grammer.GetNfa();
}

std::vector<const Terminal *> SetUnion(const std::vector<const Terminal *> &setA, const std::vector<const Terminal *> &setB)
{
	size_t iA = 0, iB = 0, sizeA = setA.size(), sizeB = setB.size();
	std::vector<const Terminal *> result;
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
bool NonTerminal::FirstSetup()
{
	std::vector<const Terminal *> result;
	for (const auto &production : productions)
		result = SetUnion(result, production.ComputeFirst());
	if (result.size() == first.size())
		return true;
	first = std::move(result);
	return false;
}
void NonTerminal::ExtractFollowConstraints(const NonTerminal *lhs, std::vector<Symbol *>::const_iterator it, std::vector<Symbol *>::const_iterator end)
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
void NonTerminal::SetupFollowConstraints() const
{
	for (auto &production : productions)
		production.FollowConstraints(this);
}
void NonTerminal::MergeFollowConstraints()
{
	std::sort(followConstraints.begin(), followConstraints.end());
	followConstraints.erase(std::unique(followConstraints.begin(), followConstraints.end()), followConstraints.end());
}
bool NonTerminal::SatisfyFollowConstraints()
{
	std::vector<const Terminal *> result = follow;
	for (auto nonTerminal : followConstraints)
		result = SetUnion(result, nonTerminal->Follow());
	if (result.size() == follow.size())
		return true;
	follow = std::move(result);
	return false;
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
	for (std::vector<Symbol *>::const_iterator it = symbols.begin(); it != symbols.end(); it++)
		(*it)->ExtractFollowConstraints(lhs, it + 1, symbols.end());
}
std::vector<const Terminal *> Production::ComputeFirst() const
{
	std::vector<const Terminal *> result;
	for (auto symbol : symbols)
	{
		result = SetUnion(result, symbol->First());
		if (!symbol->Nullable())
			break;
	}
	return result;
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

// Testing interface
void NonTerminal::AddProduction(Production &&production)
{
	productions.push_back(std::move(production));
}
Grammer::Grammer(pNonTerminal &&start)
{
	pNonTerminal extension(new NonTerminal());
	extension->AddProduction(Production({ start.get() }));
	nonTerminals.push_back(std::move(extension));
	nonTerminals.push_back(std::move(start));
}
void Grammer::AddNonTerminal(pNonTerminal &&nonTerminal)
{
	nonTerminals.push_back(std::move(nonTerminal));
}