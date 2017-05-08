#include <algorithm>
#include <fstream>
#include <iostream>
#include <memory>
#include <vector>

#define GRAMMER 3

// try to restrict access as much as possible

class Production;
class Terminal;
class NonTerminal;

std::vector<const Terminal *> SetUnion(const std::vector<const Terminal *> &setA, const std::vector<const Terminal *> &setB);

class Symbol
{
public:
	virtual bool Nullable() const = 0;
	virtual std::vector<const Terminal *> First() const = 0;
	virtual void ExtractFollowConstraints(const NonTerminal *lhs, std::vector<Symbol *>::const_iterator it, std::vector<Symbol *>::const_iterator end) = 0;
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
private:
	char c;
};
class Production
{
public:
	bool ComputeNullable() const;
	std::vector<const Terminal *> ComputeFirst() const;
	void FollowConstraints(const NonTerminal *) const;
private:
	std::vector<Symbol *> symbols;

public:
	Production(std::vector<Symbol *> &&symbols) : symbols(std::move(symbols)) {}
};
class Grammer
{
public:
	void InitializeNullable();
	void InitializeFirst();
	void InitializeFollow();

	Grammer(std::unique_ptr<NonTerminal> &&start);
	void AddNonTerminal(std::unique_ptr<NonTerminal> &&nonTerminal);
private:
	std::vector<std::unique_ptr<NonTerminal>> nonTerminals;
};

void NonTerminal::AddProduction(Production &&production)
{
	productions.push_back(std::move(production));
}
Grammer::Grammer(std::unique_ptr<NonTerminal> &&start)
{
	nonTerminals.push_back(std::move(start));
}
void Grammer::AddNonTerminal(std::unique_ptr<NonTerminal> &&nonTerminal)
{
	nonTerminals.push_back(std::move(nonTerminal));
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
	N->AddProduction(Production({ A,B }));
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