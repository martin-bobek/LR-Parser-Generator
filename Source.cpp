#include <algorithm>
#include <fstream>
#include <iostream>
#include <memory>
#include <vector>

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
};
class Terminal : public Symbol
{
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
};
class Grammer
{
public:
	void InitializeNullable();
	void InitializeFirst();
	void InitializeFollow();
private:
	std::vector<std::unique_ptr<NonTerminal>> nonTerminals;
};

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

int main()
{
	
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
		else if (setB[iB] > setA[iA])
			result.push_back(setB[iB++]);
		else
			result.push_back(setA[++iB, iA++]);
	}
}