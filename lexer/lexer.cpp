#include "llvm/ADT/ArrayRef.h"
#include "llvm/IR/Value.h"
#include <cctype>
#include <cstdlib>
#include <exception>
#include <iterator>
#include <array>
#include <bitset>
#include <cstddef>
#include <cstdint>
#include <functional>
#include <initializer_list>
#include <iostream>
#include <list>
#include <llvm/Bitcode/BitcodeWriter.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DataLayout.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/GlobalIFunc.h>
#include <llvm/IR/GlobalVariable.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/InstrTypes.h>
#include <llvm/IR/Instruction.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>
#include <llvm/Support/Error.h>
#include <llvm/Transforms/Utils/BasicBlockUtils.h>
#include <locale>
#include <ostream>
#include <queue>
#include <range/v3/algorithm/contains.hpp>
#include <range/v3/algorithm/find.hpp>
#include <range/v3/iterator/common_iterator.hpp>
#include <range/v3/iterator/concepts.hpp>
#include <range/v3/iterator/traits.hpp>
#include <range/v3/range/concepts.hpp>
#include <range/v3/range/traits.hpp>
#include <range/v3/view.hpp>
#include <range/v3/view/drop.hpp>
#include <range/v3/view/iota.hpp>
#include <range/v3/view/istream.hpp>
#include <sstream>
#include <stdexcept>
#include <stdint.h>
#include <string>
#include <string_view>
#include <tuple>
#include <type_traits>
#include <unordered_set>
#include <vector>
#include <variant>
#include <unordered_map>
#include <fstream>
#include <deque>
#include <source_location>
#include <llvm/ADT/Hashing.h>
#include <llvm/Support/FileSystem.h>
#include <string_view>
#ifdef _WIN32
#include <windows.h>
#endif

unsigned constexpr stringhash(char const* input) {
	return *input ? static_cast<unsigned int> (*input) +
		33 * stringhash(input + 1)
		: 5381;
}

constexpr inline auto operator"" _h(char const* p, size_t) {
	return stringhash(p);
}

static std::string::iterator currlexing;

extern std::string subject;

extern "C" void docall(const char* name, size_t szname, void* phashmap);

typedef std::list<std::pair<std::string, std::unordered_map<unsigned, std::string>>> record;

static size_t recording;

struct return_t {
	bool result;
	record record;
	std::unordered_map<unsigned, std::string> matches;

	bool handle(std::unordered_map<unsigned, std::string>& priormatches, ::record& currrecord) {

		currrecord.splice(currrecord.end(), record);

		if (!matches.empty()) {
			priormatches = matches;
		}

		return result;
	}
};

void doit(std::string fnname, void* phashmap, record &refrecord) {

	if (recording) {
		docall(fnname.c_str(), fnname.length(), phashmap);
	}
	else {
		refrecord.push_back({ fnname.c_str(), *static_cast<const std::unordered_map<unsigned, std::string>*>(phashmap) });
	}
}

static void replay(record &what) {

	if (recording);
	else for (const auto& call : what) {
		doit(call.first, (void*)&call.second, what);
	}

	what.clear();
}

bool consumewhitespace() {
	while (isspace(*currlexing)) currlexing++;
	return true;
}

return_t escape(std::unordered_map<unsigned, std::string> priormatches, std::unordered_map<unsigned, unsigned> flags) {

	record currrecord;

	auto doit = [&](std::string what) {
		::doit(what, (void*)&priormatches, currrecord);
	};

	if (*currlexing == '\\') {
		auto begin = ++currlexing;

		switch (*currlexing) if (0);
		else if (0) {
		case 'x':
			while (isxdigit(*++currlexing));
		}
		else if (0) {
		case '0':
			while (ranges::contains(std::string{ "01234567" }, *++currlexing));
		}
		else if (0) {
		default:
			if (std::string{ "\'\"?\\\\abfnrtv0" }.contains(*currlexing))
				++currlexing;
		}

		priormatches["escaperaw"_h] = { begin, currlexing };

		doit("addescapesequencetostring");

		assert(currlexing != begin);

		return { true, currrecord };
	}

	return { false };
}

return_t stringlit(std::unordered_map<unsigned, std::string> priormatches, std::unordered_map<unsigned, unsigned> flag) {
	consumewhitespace();

	record currrecord;

	auto doit = [&](std::string what) {
		::doit(what, (void*)&priormatches, currrecord);
	};

	auto call = [&](return_t what(std::unordered_map<unsigned, std::string>, std::unordered_map<unsigned, unsigned> flag)
) {
		return what(priormatches, flag).handle(priormatches, currrecord);
	};

	if (ranges::contains(std::array{ '\'', '\"' }, *currlexing)) {
		priormatches["begincharliteral"_h] = *currlexing++;
		auto lastnonescapebegin = currlexing;

		for (;;) {
			std::array potrawtext = { lastnonescapebegin, currlexing };

			auto checkrawcharcompletion = [&] {
				if (potrawtext[0] != potrawtext[1]) {
					priormatches["textraw"_h] = std::string{ potrawtext[0], potrawtext[1] };
					doit("addplaintexttostring");
				}
			};

			++recording;

			if (call(escape)) {
				lastnonescapebegin = currlexing;
				--recording;
				replay(currrecord);
				checkrawcharcompletion();
			}
			replay(currrecord);
			--recording;
			
			if (priormatches["begincharliteral"_h][0] == *currlexing) {
				checkrawcharcompletion();
				break;
			}
			else {
				currlexing++;
			}
		}
		doit("add_literal");
		consumewhitespace();
		return { true, currrecord };
	}

	return { false };
}

return_t numberliteral(std::unordered_map<unsigned, std::string> priormatches, std::unordered_map<unsigned, unsigned> flag) {
	consumewhitespace();

	record currrecord;

	auto doit = [&](std::string what) {
		::doit(what, (void*)&priormatches, currrecord);
	};

	auto call = [&](return_t what(std::unordered_map<unsigned, std::string>, std::unordered_map<unsigned, unsigned> flag)
) {
			return what(priormatches, flag).handle(priormatches, currrecord);
	};

	switch (stringhash(std::string{ currlexing, currlexing + 1 }.c_str())) if (0);
	else if (0) {
	case "0x"_h:
		currlexing += 2;
		auto beg = currlexing;
		while (isxdigit(*currlexing++));
		priormatches["hex"_h] = std::string{ beg , currlexing };
	}
	else if (0) {
	case "0b"_h:
		currlexing += 2;
		auto beg = currlexing;
		while (ranges::contains(std::string{"01"}, *currlexing++));
		priormatches["bin"_h] = std::string{ beg , currlexing };
	}
	else if (0) {
	default:
		if (*currlexing == '0' && isdigit(currlexing[1])) {
			auto beg = currlexing;
			currlexing += 2;
			while (ranges::contains(std::string{ "01234567" }, *++currlexing));
			priormatches["oct"_h] = std::string{ beg , currlexing };
		}
		else if (isdigit(*currlexing)) {
			auto beg = currlexing;
			while (isdigit(*++currlexing));
			priormatches["dec"_h] = std::string{ beg , currlexing };
		}
		else {
			return { false };
		}
	}

	{
		auto beg = currlexing;

		if (ranges::contains(std::string{ "lL" }, *currlexing)) {
			++currlexing;
			if (*currlexing == currlexing[-1])
				++currlexing;
			if (ranges::contains(std::string{ "uU" }, *currlexing)) {
				++currlexing;
			}
		}
		else if (ranges::contains(std::string{ "uU" }, *currlexing)) {
			++currlexing;
			if (ranges::contains(std::string{ "lL" }, *currlexing)) {
				++currlexing;
				if (*currlexing == currlexing[-1])
					++currlexing;
			}
		}

		if (beg != currlexing) {
			priormatches["lng"_h] = std::string{ beg , currlexing };
		}

		doit("num_lit");

		consumewhitespace();

		return { true, currrecord };
	}
}

return_t exponent(std::unordered_map<unsigned, std::string> priormatches, std::unordered_map<unsigned, unsigned> flag) {


	record currrecord;

	auto doit = [&](std::string what) {
		::doit(what, (void*)&priormatches, currrecord);
	};

	auto call = [&](return_t what(std::unordered_map<unsigned, std::string>, std::unordered_map<unsigned, unsigned> flag)
) {
			return what(priormatches, flag).handle(priormatches, currrecord);
	};

	if (ranges::contains(std::string{ "eE" }, *currlexing)) {
		auto beg = ++currlexing;
		if (ranges::contains(std::string{ "+-" }, *currlexing)) {
			priormatches["signexp"_h] = std::string{ beg, ++currlexing };
		}
		beg = currlexing;
		while (isdigit(*currlexing++));
		priormatches["exponent"_h] = std::string{ beg , currlexing };

		return { true, currrecord, priormatches };
	}

	return { false };
}

return_t floating(std::unordered_map<unsigned, std::string> priormatches, std::unordered_map<unsigned, unsigned> flag) {
	consumewhitespace();

	record currrecord;

	auto doit = [&](std::string what) {
		::doit(what, (void*)&priormatches, currrecord);
	};

	auto call = [&](return_t what(std::unordered_map<unsigned, std::string>, std::unordered_map<unsigned, unsigned> flag)
) {
			return what(priormatches, flag).handle(priormatches, currrecord);
	};

	if(*currlexing == '.') 
fraction:
	{
		auto beg = ++currlexing;
		while (isdigit(*currlexing++));
		priormatches["fraction"_h] = std::string{ beg , currlexing };
		call(exponent);
		goto success;
	}
	else if (isdigit(*currlexing)) {
		auto beg = currlexing;
		while (isdigit(*++currlexing));
		priormatches["whole"_h] = std::string{ beg , currlexing };
		if (*currlexing == '.') {
			goto fraction;
		}
		else if(call(exponent)) {
			goto success;
		}
	}
	else if (std::string{ currlexing, currlexing + 3 } == "NAN") {
		priormatches["nan"_h] = std::string{ currlexing, currlexing + 3 };
		currlexing += 3;
		goto success;
	}
	return { false };
success:
	{
		auto beg = currlexing;

		if (ranges::contains(std::string{ "flFL" }, *currlexing)) ++currlexing;

		if (beg != currlexing) {
			priormatches["suffixflt"_h] = std::string{ beg , currlexing };
		}

		doit("collect_float_literal");

		consumewhitespace();

		return { true, currrecord };
	}
}

bool isonboundary(std::string::iterator what = currlexing) {
	return (isalnum(what[-1]) || what[-1] == '_') != (isalnum(what[0]) || what[0] == '_');
}

return_t identifier(std::unordered_map<unsigned, std::string> priormatches, std::unordered_map<unsigned, unsigned> flag) {
	consumewhitespace();

	if (isonboundary() && (isalpha(*currlexing) || *currlexing == '_')) {
		auto beg = currlexing;
		++currlexing;
		while (isalnum(*currlexing) || *currlexing == '_') ++currlexing;
		priormatches["ident"_h] = std::string{ beg , currlexing };
		consumewhitespace();

		return { true, {}, priormatches };
	}

	return { false };
}

return_t qualifiersidentifier(std::unordered_map<unsigned, std::string> priormatches, std::unordered_map<unsigned, unsigned> flag) {
	consumewhitespace();

	record currrecord;

	auto doit = [&](std::string what) {
		::doit(what, (void*)&priormatches, currrecord);
	};

	auto call = [&](return_t what(std::unordered_map<unsigned, std::string>, std::unordered_map<unsigned, unsigned> flag)
) {
			return what(priormatches, flag).handle(priormatches, currrecord);
	};

	auto last = currlexing;

	if (call(identifier)) {
		switch (stringhash(priormatches["ident"_h].c_str())) if (0);
		else if (0) {
		case "const"_h:
		case "restrict"_h:
		case "volatile"_h:
		case "extern"_h:
		case "auto"_h:
		case "static"_h:
		case "register"_h:
			priormatches["qualiffound"_h] = std::move(priormatches["ident"_h]);
			doit("add_qualif");
			consumewhitespace();

			return { true, currrecord };
		}
	}

	currlexing = last;

	return { false };
}

return_t qualifiersortypeidentifier(std::unordered_map<unsigned, std::string> priormatches, std::unordered_map<unsigned, unsigned> flag) {
	consumewhitespace();

	record currrecord;

	auto doit = [&](std::string what) {
		::doit(what, (void*)&priormatches, currrecord);
	};

	auto call = [&](return_t what(std::unordered_map<unsigned, std::string>, std::unordered_map<unsigned, unsigned> flag)
		) {
			return what(priormatches, flag).handle(priormatches, currrecord);
	};

	auto last = currlexing;

	if (call(identifier)) {
		switch (stringhash(priormatches["ident"_h].c_str())) if (0);
		else if (0) {
		case "const"_h:
		case "restrict"_h:
		case "volatile"_h:
		case "extern"_h:
		case "auto"_h:
		case "static"_h:
		case "register"_h:
			priormatches["qualiffound"_h] = std::move(priormatches["ident"_h]);
			doit("add_qualif");
			consumewhitespace();

			return { true, currrecord, priormatches };
		}
		else if (0) {
		case "int"_h:
		case "char"_h:
		case "short"_h:
		case "long"_h:
		case "signed"_h:
		case "unsigned"_h:
		case "float"_h:
		case "double"_h:
			priormatches["typefound"_h] = std::move(priormatches["ident"_h]);
			doit("add_type");
			consumewhitespace();

			return { true, currrecord, priormatches };
		}
	}

	currlexing = last;

	return { false };
}

return_t Tinside(std::unordered_map<unsigned, std::string> priormatches, std::unordered_map<unsigned, unsigned> flag);
return_t abstrsubs(std::unordered_map<unsigned, std::string> priormatches, std::unordered_map<unsigned, unsigned> flag);

return_t abstrptrrev(std::unordered_map<unsigned, std::string> priormatches, std::unordered_map<unsigned, unsigned> flag) {
	consumewhitespace();

	record currrecord;

	auto doit = [&](std::string what) {
		::doit(what, (void*)&priormatches, currrecord);
	};

	auto call = [&](return_t what(std::unordered_map<unsigned, std::string>, std::unordered_map<unsigned, unsigned> flag)
) {
			return what(priormatches, flag).handle(priormatches, currrecord);
	};

	if (*currlexing == '*') {
		currlexing++;
		consumewhitespace();
		const std::string stdcallliteral = "__stdcall";
		if (isonboundary() && 
			std::string{ currlexing , currlexing + stdcallliteral.length() } == stdcallliteral &&
			isonboundary(currlexing + stdcallliteral.length())) {

			currlexing += stdcallliteral.length();

			priormatches["callconv"_h] = stdcallliteral;

			doit("setcallconv");

			consumewhitespace();
		}

		size_t currrecordsize = currrecord.size();

		++recording;

		while (call(qualifiersidentifier));

		--recording;

		currrecord.resize(currrecordsize);

		if (call(abstrptrrev)) {
			doit("addptrtotype");
		}
		else {
			assert(call(Tinside));
			while (call(abstrsubs));
			doit("addptrtotype");
		}
	}
}

return_t Tabstrrestalt(std::unordered_map<unsigned, std::string> priormatches, std::unordered_map<unsigned, unsigned> flag) {

	record currrecord;

	auto doit = [&](std::string what) {
		::doit(what, (void*)&priormatches, currrecord);
	};

	auto call = [&](return_t what(std::unordered_map<unsigned, std::string>, std::unordered_map<unsigned, unsigned> flag)
) {
			return what(priormatches, flag).handle(priormatches, currrecord);
	};

	auto callfwd = [&](return_t what(std::unordered_map<unsigned, std::string>, std::unordered_map<unsigned, unsigned> flag)
) {
			return what(priormatches, flag);
	};
	
	switch (flag["outter"_h]) if (0);
	else if (0) {
	case "params"_h:
		++recording;
		doit("identifier_decl");
		if (call(abstrsubs)) {
			--recording;
			replay(currrecord);
			while (call(abstrsubs));
			return { true, currrecord };
		}
		currrecord.pop_back();
		--recording;
		if (!call(Tinside)) {
			return { false };
		}

		while (call(abstrsubs));
		return { true, currrecord };
	}
	else if (0) {
	case "normal"_h:
		if (!call(Tinside)) {
			return { false };
		}

		while (call(abstrsubs));
		return { true, currrecord };
	}
	else if (0) {
	case "optional"_h:
		if (!call(Tinside)) {
			if (call(abstrsubs)) while (call(abstrsubs));
			else return { false };
			return { true, currrecord };
		}

		while (call(abstrsubs));
		return { true, currrecord };
	}

	assert(0);  "invocation without proper set flag!";
}

std::list<std::unordered_map<unsigned, unsigned>> identifiersmap( 1 );

bool isidentkeyword(std::string ident) {
	switch(stringhash(ident.c_str()))
	case "typedef"_h:
	case "struct"_h:
	case "enum"_h:
	case "sizeof"_h:
	case "break"_h:
	case "case"_h:
	case "continue"_h:
	case "default"_h:
	case "do"_h:
	case "else"_h:
	case "for"_h:
	case "goto"_h:
	case "if"_h:
	case "return"_h:
	case "switch"_h:
	case "while"_h:
	case "int"_h:
	case "char"_h:
	case "short"_h:
	case "long"_h:
	case "signed"_h:
	case "unsigned"_h:
	case "float"_h:
	case "double"_h:
	case "const"_h:
	case "restrict"_h:
	case "volatile"_h:
	case "extern"_h:
	case "auto"_h:
	case "static"_h:
	case "register"_h:
		return true;

	return false;
}

return_t identifierminedecl(std::unordered_map<unsigned, std::string> priormatches, std::unordered_map<unsigned, unsigned> flag) {
	consumewhitespace();

	record currrecord;

	auto doit = [&](std::string what) {
		::doit(what, (void*)&priormatches, currrecord);
	};

	auto call = [&](return_t what(std::unordered_map<unsigned, std::string>, std::unordered_map<unsigned, unsigned> flag)
) {
			return what(priormatches, flag).handle(priormatches, currrecord);
	};

	auto last = currlexing;

	if (call(identifier) && !isidentkeyword(priormatches["ident"_h])) {
		doit("identifier_decl");
		consumewhitespace();

		if (flag["optinit"_h] != "bitfl"_h) {
			identifiersmap.back()[stringhash(priormatches["ident"_h].c_str())] = priormatches.contains("typedefkey"_h);
		}

		return { true, currrecord };
	}

	currlexing = last;

	return { false };
}

return_t ident(std::unordered_map<unsigned, std::string> priormatches, std::unordered_map<unsigned, unsigned> flag) {
	consumewhitespace();

	record currrecord;

	auto doit = [&](std::string what) {
		::doit(what, (void*)&priormatches, currrecord);
	};

	auto call = [&](return_t what(std::unordered_map<unsigned, std::string>, std::unordered_map<unsigned, unsigned> flag)
		) {
			return what(priormatches, flag).handle(priormatches, currrecord);
	};

	auto last = currlexing;

	if (call(identifier) && !isidentkeyword(priormatches["ident"_h])) {
		doit("obtainvalbyidentifier");
		consumewhitespace();

		return { true, currrecord };
	}

	currlexing = last;

	return { false };
}

return_t abstdecl(std::unordered_map<unsigned, std::string> priormatches, std::unordered_map<unsigned, unsigned> flag) {
	consumewhitespace();

	record currrecord;

	auto doit = [&](std::string what) {
		::doit(what, (void*)&priormatches, currrecord);
	};

	auto call = [&](return_t what(std::unordered_map<unsigned, std::string>, std::unordered_map<unsigned, unsigned> flag)
) {
			return what(priormatches, flag).handle(priormatches, currrecord);
	};

	auto callfwd = [&](return_t what(std::unordered_map<unsigned, std::string>, std::unordered_map<unsigned, unsigned> flag)
) {
			return what(priormatches, flag);
	};

	const std::string stdcallliteral = "__stdcall";
	if (isonboundary() &&
		std::string{ currlexing , currlexing + stdcallliteral.length() } == stdcallliteral &&
		isonboundary(currlexing + stdcallliteral.length())) {
		currlexing += stdcallliteral.length();

		priormatches["callconv"_h] = stdcallliteral;

		doit("setcallconv");

		consumewhitespace();
	}

	if (!call(abstrptrrev)) {
		return callfwd(Tabstrrestalt);
	}
	else {
		return { true, currrecord };
	}

	return { false };
}


return_t Tinside(std::unordered_map<unsigned, std::string> priormatches, std::unordered_map<unsigned, unsigned> flag) {

	record currrecord;

	auto doit = [&](std::string what) {
		::doit(what, (void*)&priormatches, currrecord);
	};

	auto call = [&](return_t what(std::unordered_map<unsigned, std::string>, std::unordered_map<unsigned, unsigned> flag)
) {
			return what(priormatches, flag).handle(priormatches, currrecord);
	};

	auto callfwd = [&](return_t what(std::unordered_map<unsigned, std::string>, std::unordered_map<unsigned, unsigned> flag)
) {
			return what(priormatches, flag);
	};

	switch (flag["outter"_h]) if (0);
	else if (0) {
	case "params"_h:
		if (*currlexing == '(') {
			call(abstdecl);
			assert(*currlexing == ')');
			++currlexing;
			consumewhitespace();
		}
		else {
			if (!call(identifierminedecl)) {
				doit("identifier_decl");
			}
		}

		return { true, currrecord };
	}
	else if (0) {
	case "normal"_h:
		if (*currlexing == '(') {
			call(abstdecl);
			assert(*currlexing == ')');
			++currlexing;
			consumewhitespace();
			return { true, currrecord };
		}
		else {
			return callfwd(identifierminedecl);
		}
	}
	else if (0) {
	case "optional"_h:
		if (*currlexing == '(') {
			call(abstdecl);
			assert(*currlexing == ')');
			++currlexing;
			consumewhitespace();
			return { true, currrecord };
		}
		
		return { true, currrecord };
	}

	assert(0);  "invocation without proper set flag!";
}

return_t primexpr(std::unordered_map<unsigned, std::string> priormatches, std::unordered_map<unsigned, unsigned> flag);
return_t primexprcall(std::unordered_map<unsigned, std::string> priormatches, std::unordered_map<unsigned, unsigned> flag);
return_t param(std::unordered_map<unsigned, std::string> priormatches, std::unordered_map<unsigned, unsigned> flag);

return_t abstrsubs(std::unordered_map<unsigned, std::string> priormatches, std::unordered_map<unsigned, unsigned> flag) {
	consumewhitespace();

	record currrecord;

	auto doit = [&](std::string what) {
		::doit(what, (void*)&priormatches, currrecord);
	};

	auto call = [&](return_t what(std::unordered_map<unsigned, std::string>, std::unordered_map<unsigned, unsigned> flag)
) {
			return what(priormatches, flag).handle(priormatches, currrecord);
	};

	if (*currlexing == '[') {
		++currlexing;
		doit("beginconstantexpr");
		call(primexpr);
		assert(*currlexing == ']');
		++currlexing;
		doit("addsubtotype");
		doit("endconstantexpr");
		consumewhitespace();
		return { true, currrecord };
	}
	else if (*currlexing == '(') {
		++currlexing;
		consumewhitespace();
		doit("startfunctionparamdecl");

		if (std::string{ currlexing , currlexing + 3 } == "...") {
			priormatches["rest"_h] = std::string{ currlexing , currlexing + 3 };
			currlexing += 3;
			goto noparams;
		}
		else if (isonboundary() && std::string{ currlexing , currlexing + 4 } == "void" && isonboundary(currlexing + 4)) {
			currlexing += 4;
			goto noparams;
		}

		while (call(param));

		if (std::string{ currlexing , currlexing + 3 } == "...") {
			priormatches["rest"_h] = std::string{ currlexing , currlexing + 3 };
			currlexing += 3;
		}

		noparams:
		{
			consumewhitespace();

			assert(*currlexing == ')');

			++currlexing;

			doit("endfunctionparamdecl");

			consumewhitespace();

			return { true, currrecord };
		}
	}

	return { false };
}

return_t identifier_typedef(std::unordered_map<unsigned, std::string> priormatches, std::unordered_map<unsigned, unsigned> flag) {
	consumewhitespace();

	record currrecord;

	auto doit = [&](std::string what) {
		::doit(what, (void*)&priormatches, currrecord);
	};

	auto call = [&](return_t what(std::unordered_map<unsigned, std::string>, std::unordered_map<unsigned, unsigned> flag)
		) {
			return what(priormatches, flag).handle(priormatches, currrecord);
	};

	auto last = currlexing;

	if (priormatches.contains("typedefnmmatched"_h)) {
		return { false };
	}

	if (call(identifier) && identifiersmap.back()[stringhash(priormatches["ident"_h].c_str())]) {
		consumewhitespace();

		priormatches["typedefnmmatched"_h] = std::move(priormatches["ident"_h]);

		return { true, currrecord };
	}

	return { false };
}

return_t typedefkey(std::unordered_map<unsigned, std::string> priormatches, std::unordered_map<unsigned, unsigned> flag) {
	consumewhitespace();

	record currrecord;

	auto doit = [&](std::string what) {
		::doit(what, (void*)&priormatches, currrecord);
	};

	auto call = [&](return_t what(std::unordered_map<unsigned, std::string>, std::unordered_map<unsigned, unsigned> flag)
		) {
			return what(priormatches, flag).handle(priormatches, currrecord);
	};

	const std::string typedefkeyliteral = "typedef";
	if (isonboundary() &&
		std::string{ currlexing , currlexing + typedefkeyliteral.length() } == typedefkeyliteral &&
		isonboundary(currlexing + typedefkeyliteral.length())) {

		currlexing += typedefkeyliteral.length();

		priormatches["typedefkey"_h] = typedefkeyliteral;

		consumewhitespace();

		return { true, currrecord, priormatches };
	}

	return { false };
}

return_t assignorsomething(std::unordered_map<unsigned, std::string> priormatches, std::unordered_map<unsigned, unsigned> flag);

return_t enumerator(std::unordered_map<unsigned, std::string> priormatches, std::unordered_map<unsigned, unsigned> flag) {
	consumewhitespace();
	record currrecord;

	auto doit = [&](std::string what) {
		::doit(what, (void*)&priormatches, currrecord);
	};

	auto call = [&](return_t what(std::unordered_map<unsigned, std::string>, std::unordered_map<unsigned, unsigned> flag)
		) {
			return what(priormatches, flag).handle(priormatches, currrecord);
	};

	auto callfwd = [&](return_t what(std::unordered_map<unsigned, std::string>, std::unordered_map<unsigned, unsigned> flag)
		) {
			return what(priormatches, flag);
	};

	if (call(identifier)) {
		doit("add_ident_to_enum_def");
		if (*currlexing == '=') {
			doit("beginconstantexpr");
			call(assignorsomething);
			doit("end_ass_to_enum_def");
		}
		else {
			doit("end_without_ass_to_enum_def");
		}

		return { true, currrecord };
	}

	return { false };
}

return_t strcelem(std::unordered_map<unsigned, std::string> priormatches, std::unordered_map<unsigned, unsigned> flag) {
	priormatches.clear();
	record currrecord;

	auto doit = [&](std::string what) {
		::doit(what, (void*)&priormatches, currrecord);
	};

	auto call = [&](return_t what(std::unordered_map<unsigned, std::string>, std::unordered_map<unsigned, unsigned> flag)
		) {
			return what(priormatches, flag).handle(priormatches, currrecord);
	};

	auto callfwd = [&](return_t what(std::unordered_map<unsigned, std::string>, std::unordered_map<unsigned, unsigned> flag)
		) {
			return what(priormatches, flag);
	};

	flag["optinit"_h] = "bitfl"_h;
	flag["outter"_h] = "normal"_h;
	flag["opt"_h] = false;

	if (auto res = callfwd(abstdeclorallqualifs); res.result) {
		consumewhitespace();
		assert(*currlexing == ';');
		++currlexing;
		consumewhitespace();
		return res;
	}
	return { false };
}

return_t structorunion(std::unordered_map<unsigned, std::string> priormatches, std::unordered_map<unsigned, unsigned> flag) {
	consumewhitespace();
	record currrecord;

	auto doit = [&](std::string what) {
		::doit(what, (void*)&priormatches, currrecord);
	};

	auto call = [&](return_t what(std::unordered_map<unsigned, std::string>, std::unordered_map<unsigned, unsigned> flag)
		) {
			return what(priormatches, flag).handle(priormatches, currrecord);
	};

	auto callfwd = [&](return_t what(std::unordered_map<unsigned, std::string>, std::unordered_map<unsigned, unsigned> flag)
		) {
			return what(priormatches, flag);
	};

	if (priormatches.contains("structorunionlast"_h) || priormatches.contains("enum"_h)) {
		return { false };
	}

	const std::string strucliteral = "struct";
	const std::string unionliteral = "union";
	const std::string enumliteral = "enum";

	bool isenum = false;

	if (isonboundary() &&
		std::string{ currlexing , currlexing + unionliteral.length() } == unionliteral &&
		isonboundary(currlexing + unionliteral.length())) {
		currlexing += unionliteral.length();
		priormatches["structorunionlast"_h] = unionliteral;
	}
	else if (isonboundary() &&
		std::string{ currlexing , currlexing + strucliteral.length() } == strucliteral &&
		isonboundary(currlexing + strucliteral.length())) {
		currlexing += strucliteral.length();
		priormatches["structorunionlast"_h] = strucliteral;
	}
	else if (isonboundary() &&
		std::string{ currlexing , currlexing + enumliteral.length() } == enumliteral &&
		isonboundary(currlexing + enumliteral.length())) {
		currlexing += enumliteral.length();
		priormatches["enum"_h] = enumliteral;
		isenum = true;
	}

	consumewhitespace();

	auto last = currlexing;

	bool hastag = true;

	if (call(identifier) && !isidentkeyword(priormatches["ident"_h])) {
		consumewhitespace();
		priormatches["lasttag"_h] = std::move(priormatches["ident"_h]);
	}
	else {
		hastag = false;
		currlexing = last;
	}

	if (*currlexing == '{') {
		++currlexing;
		consumewhitespace();

		size_t oldrecord = recording;

		recording = 0;

		if (isenum) {
			if (call(enumerator)) {
				if (*currlexing == ',') {
					while (call(enumerator) && *currlexing == ',')
						++currlexing;
				}
			}

			assert(*currlexing == '}');

			currlexing++;

			consumewhitespace();

			recording = oldrecord;

			return { true };
		}
		else {
			doit("struc_or_union_body");

			while (call(strcelem));

			assert(*currlexing == '}');

			doit("endbuildingstructorunion");

			currlexing++;

			consumewhitespace();

			recording = oldrecord;

			return { true, {},  priormatches };
		}
	}

	if (!hastag) {

		return { false };
	}

	return { true, {},  priormatches };
}

return_t abstdeclorallqualifsqualifs(std::unordered_map<unsigned, std::string> priormatches, std::unordered_map<unsigned, unsigned> flag) {
	record currrecord;

	auto doit = [&](std::string what) {
		::doit(what, (void*)&priormatches, currrecord);
	};

	auto call = [&](return_t what(std::unordered_map<unsigned, std::string>, std::unordered_map<unsigned, unsigned> flag)
		) {
			return what(priormatches, flag).handle(priormatches, currrecord);
	};

	auto callfwd = [&](return_t what(std::unordered_map<unsigned, std::string>, std::unordered_map<unsigned, unsigned> flag)
		) {
			return what(priormatches, flag);
	};

	if (auto res = callfwd(qualifiersortypeidentifier); res.result) {
		return res;
	}

	if (!priormatches.contains("qualiffound"_h) && !priormatches.contains("typefound"_h)) {

		if (auto res = callfwd(structorunion); res.result) {
			return res;
		}
		else if (auto res = callfwd(identifier_typedef); res.result) {
			return res;
		}
	}

	if (auto res = callfwd(typedefkey); res.result) {
		return res;
	}

	return { false };
}

return_t abstrbitfield(std::unordered_map<unsigned, std::string> priormatches, std::unordered_map<unsigned, unsigned> flag) {
	consumewhitespace();
	record currrecord;

	auto doit = [&](std::string what) {
		::doit(what, (void*)&priormatches, currrecord);
	};

	auto call = [&](return_t what(std::unordered_map<unsigned, std::string>, std::unordered_map<unsigned, unsigned> flag)
		) {
			return what(priormatches, flag).handle(priormatches, currrecord);
	};

	auto callfwd = [&](return_t what(std::unordered_map<unsigned, std::string>, std::unordered_map<unsigned, unsigned> flag)
		) {
			return what(priormatches, flag);
	};

	if (*currlexing == ':') {
		++currlexing;
		consumewhitespace();
		doit("beginconstantexpr");
		auto ret = callfwd(assignorsomething);
		doit("endconstantexpr");

		return ret;
	}

	return { false };
}

return_t designator(std::unordered_map<unsigned, std::string> priormatches, std::unordered_map<unsigned, unsigned> flag) {
	consumewhitespace();
	record currrecord;

	auto doit = [&](std::string what) {
		::doit(what, (void*)&priormatches, currrecord);
	};

	auto call = [&](return_t what(std::unordered_map<unsigned, std::string>, std::unordered_map<unsigned, unsigned> flag)
		) {
			return what(priormatches, flag).handle(priormatches, currrecord);
	};

	auto callfwd = [&](return_t what(std::unordered_map<unsigned, std::string>, std::unordered_map<unsigned, unsigned> flag)
		) {
			return what(priormatches, flag);
	};

	if (*currlexing == '[') {
		++currlexing;
		consumewhitespace();
		doit("beginconstantexpr");
		auto ret = callfwd(assignorsomething);
		doit("subobject");
		doit("endconstantexpr");

		assert(*currlexing == ']');
		++currlexing;
		consumewhitespace();
		return ret;
	}
	else if (*currlexing == '.') {
		++currlexing;
		consumewhitespace();
		auto ret = callfwd(identifier);
		doit("submember");
		return ret;
	}

	return { false };
}

return_t initializer(std::unordered_map<unsigned, std::string> priormatches, std::unordered_map<unsigned, unsigned> flag) {
	consumewhitespace();
	record currrecord;

	auto doit = [&](std::string what) {
		::doit(what, (void*)&priormatches, currrecord);
	};

	auto call = [&](return_t what(std::unordered_map<unsigned, std::string>, std::unordered_map<unsigned, unsigned> flag)
		) {
			return what(priormatches, flag).handle(priormatches, currrecord);
	};

	auto callfwd = [&](return_t what(std::unordered_map<unsigned, std::string>, std::unordered_map<unsigned, unsigned> flag)
		) {
			return what(priormatches, flag);
	};

	if (call(assignorsomething)) {
		doit("extract");
	}
	else if (*currlexing == '{') {
		++currlexing;
		consumewhitespace();
		if (call(designator)) {
			while (call(designator));
			assert(*currlexing == '=');
			++currlexing;
		}

		if (call(initializer)) {
			do {
				if (*currlexing != ',') break;
				currlexing++;
			} while (call(initializer));
		}

		assert(*currlexing == '}');

		++currlexing;

		consumewhitespace();

		return { true, currrecord };
	}

	return { false };
}

return_t abstrinitialization(std::unordered_map<unsigned, std::string> priormatches, std::unordered_map<unsigned, unsigned> flag) {
	consumewhitespace();
	record currrecord;

	auto doit = [&](std::string what) {
		::doit(what, (void*)&priormatches, currrecord);
	};

	auto call = [&](return_t what(std::unordered_map<unsigned, std::string>, std::unordered_map<unsigned, unsigned> flag)
		) {
			return what(priormatches, flag).handle(priormatches, currrecord);
	};

	auto callfwd = [&](return_t what(std::unordered_map<unsigned, std::string>, std::unordered_map<unsigned, unsigned> flag)
		) {
			return what(priormatches, flag);
	};

	if (*currlexing == '=') {
		++currlexing;
		consumewhitespace();
		doit("begin_initializer");
		auto ret = callfwd(initializer);
		doit("finalize_initializer");

		return ret;
	}

	return { false };
}

return_t optinit(std::unordered_map<unsigned, std::string> priormatches, std::unordered_map<unsigned, unsigned> flag) {
	record currrecord;

	auto doit = [&](std::string what) {
		::doit(what, (void*)&priormatches, currrecord);
	};

	auto call = [&](return_t what(std::unordered_map<unsigned, std::string>, std::unordered_map<unsigned, unsigned> flag)
		) {
			return what(priormatches, flag).handle(priormatches, currrecord);
	};

	auto callfwd = [&](return_t what(std::unordered_map<unsigned, std::string>, std::unordered_map<unsigned, unsigned> flag)
		) {
			return what(priormatches, flag);
	};

	switch (flag["optinit"_h]) if (0);
	else if (0) {
	case "bifl"_h:
		return callfwd(abstrbitfield);
	}
	else if (0) {
	case "init"_h:
		return callfwd(abstrinitialization);
	}

	return { false };
}

return_t abstdeclorallqualifs(std::unordered_map<unsigned, std::string> priormatches, std::unordered_map<unsigned, unsigned> flag) {
	consumewhitespace();
	record currrecord;

	auto doit = [&](std::string what) {
		::doit(what, (void*)&priormatches, currrecord);
	};

	auto call = [&](return_t what(std::unordered_map<unsigned, std::string>, std::unordered_map<unsigned, unsigned> flag)
		) {
			return what(priormatches, flag).handle(priormatches, currrecord);
	};

	auto callfwd = [&](return_t what(std::unordered_map<unsigned, std::string>, std::unordered_map<unsigned, unsigned> flag)
		) {
			return what(priormatches, flag);
	};

	++recording;
	
	if (flag["opt"_h]) goto startwhile;

	if (call(abstdeclorallqualifsqualifs)) {
	startwhile:
		while (call(abstdeclorallqualifsqualifs));
	}
	else {
		--recording;
		return { false };
	}

	auto qualifrecord = currrecord;

	currrecord.clear();

	--recording;

	if (flag["outter"_h] == "optional"_h) {
		doit("identifier_decl");
		goto startdowhile;
	}

	if (call(abstdecl)) {
		do {
			doit("enddeclaration");
			replay(qualifrecord);
			doit("endqualifs");
			call(optinit);
			if (*currlexing != ',' || flag["outter"_h] != "normal"_h) break;
			++currlexing;
startdowhile:
		} while (call(abstdecl));

		return { true, currrecord };
	}
	else if (priormatches.contains("structorunionlast"_h)) {
		doit("check_stray_struc");
		return { true, currrecord };
	}
	else if (priormatches.contains("enum"_h)) {
		return { true, currrecord };
	}

	return { false };
}

return_t param(std::unordered_map<unsigned, std::string> priormatches, std::unordered_map<unsigned, unsigned> flag) {
	priormatches.clear();
	record currrecord;

	auto doit = [&](std::string what) {
		::doit(what, (void*)&priormatches, currrecord);
	};

	auto call = [&](return_t what(std::unordered_map<unsigned, std::string>, std::unordered_map<unsigned, unsigned> flag)
) {
			return what(priormatches, flag).handle(priormatches, currrecord);
	};

	auto callfwd = [&](return_t what(std::unordered_map<unsigned, std::string>, std::unordered_map<unsigned, unsigned> flag)
) {
			return what(priormatches, flag);
	};

	flag["outter"_h] = "params"_h;
	flag["opt"_h] = false;
	flag["optinit"_h] = "none"_h;

	return callfwd(abstdeclorallqualifs);
}

return_t typename_(std::unordered_map<unsigned, std::string> priormatches, std::unordered_map<unsigned, unsigned> flag) {
	consumewhitespace();
	priormatches.clear();
	record currrecord;

	auto doit = [&](std::string what) {
		::doit(what, (void*)&priormatches, currrecord);
	};

	auto call = [&](return_t what(std::unordered_map<unsigned, std::string>, std::unordered_map<unsigned, unsigned> flag)
		) {
			return what(priormatches, flag).handle(priormatches, currrecord);
	};

	auto callfwd = [&](return_t what(std::unordered_map<unsigned, std::string>, std::unordered_map<unsigned, unsigned> flag)
		) {
			return what(priormatches, flag);
	};

	flag["outter"_h] = "optional"_h;
	flag["opt"_h] = false;
	flag["optinit"_h] = "none"_h;

	return callfwd(abstdeclorallqualifs);
}

return_t inner(std::unordered_map<unsigned, std::string> priormatches, std::unordered_map<unsigned, unsigned> flag) {
	consumewhitespace();
	priormatches.clear();
	record currrecord;

	auto doit = [&](std::string what) {
		::doit(what, (void*)&priormatches, currrecord);
	};

	auto call = [&](return_t what(std::unordered_map<unsigned, std::string>, std::unordered_map<unsigned, unsigned> flag)
		) {
			return what(priormatches, flag).handle(priormatches, currrecord);
	};

	auto callfwd = [&](return_t what(std::unordered_map<unsigned, std::string>, std::unordered_map<unsigned, unsigned> flag)
		) {
			return what(priormatches, flag);
	};

	return {call(stringlit) || call(floating) || call(numberliteral) || call(numberliteral) || call(ident), currrecord};
}

return_t inparenths(std::unordered_map<unsigned, std::string> priormatches, std::unordered_map<unsigned, unsigned> flag) {
	consumewhitespace();
	priormatches.clear();
	record currrecord;

	auto doit = [&](std::string what) {
		::doit(what, (void*)&priormatches, currrecord);
	};

	auto call = [&](return_t what(std::unordered_map<unsigned, std::string>, std::unordered_map<unsigned, unsigned> flag)
		) {
			return what(priormatches, flag).handle(priormatches, currrecord);
	};

	auto callfwd = [&](return_t what(std::unordered_map<unsigned, std::string>, std::unordered_map<unsigned, unsigned> flag)
		) {
			return what(priormatches, flag);
	};

	if (*currlexing == '(') {
		++currlexing;
		call(primexpr);
		assert(*currlexing == ')');
		++currlexing;
		consumewhitespace();
		return { true, currrecord };
	}

	return { false };
}

return_t postfix(std::unordered_map<unsigned, std::string> priormatches, std::unordered_map<unsigned, unsigned> flag) {
	consumewhitespace();
	priormatches.clear();
	record currrecord;

	auto doit = [&](std::string what) {
		::doit(what, (void*)&priormatches, currrecord);
	};

	auto call = [&](return_t what(std::unordered_map<unsigned, std::string>, std::unordered_map<unsigned, unsigned> flag)
		) {
			return what(priormatches, flag).handle(priormatches, currrecord);
	};

	auto callfwd = [&](return_t what(std::unordered_map<unsigned, std::string>, std::unordered_map<unsigned, unsigned> flag)
		) {
			return what(priormatches, flag);
	};

	switch (*currlexing) if (0);
	else if (0) {
	case '[':
		call(primexpr);
		assert(*currlexing == ']');
		doit("subscript");
		++currlexing;
		consumewhitespace();
		return { true, currrecord };
	}
	else if (0) {
	case '(':
		doit("startfunctioncall");
		call(primexprcall);
		assert(*currlexing == ')');
		doit("endfunctioncall");
		++currlexing;
		consumewhitespace();
		return { true, currrecord };
	}

	if (*currlexing == '.') {
		priormatches["arrowordotraw"_h] = '.';
		++currlexing;
		goto memberaccess;
	}
	else if (std::string{ currlexing, currlexing + 2 } == "->") {
		priormatches["arrowordotraw"_h] = "->";
		currlexing += 2;
	memberaccess:
		call(identifier);
		doit("memberaccess");

		return { true, currrecord };
	}
	else if (ranges::contains(std::array{ "++"_h, "--"_h }, stringhash(std::string{ currlexing, currlexing + 2 }.c_str()))) {
		priormatches["postfixarithraw"_h] = std::string{ currlexing, currlexing + 2 };
		currlexing += 2;
		doit("unaryincdec");

		return { true, currrecord };
	}

	return { false };
}

return_t unary(std::unordered_map<unsigned, std::string> priormatches, std::unordered_map<unsigned, unsigned> flag) {
	consumewhitespace();
	priormatches.clear();
	record currrecord;

	auto doit = [&](std::string what) {
		::doit(what, (void*)&priormatches, currrecord);
	};

	auto call = [&](return_t what(std::unordered_map<unsigned, std::string>, std::unordered_map<unsigned, unsigned> flag)
		) {
			return what(priormatches, flag).handle(priormatches, currrecord);
	};

	auto callfwd = [&](return_t what(std::unordered_map<unsigned, std::string>, std::unordered_map<unsigned, unsigned> flag)
		) {
			return what(priormatches, flag);
	};

	if (ranges::contains(std::array{ "++"_h, "--"_h }, stringhash(std::string{ currlexing, currlexing + 2 }.c_str()))) {
		priormatches["prefixarithraw"_h] = std::string{ currlexing, currlexing + 2 };
		currlexing += 2;
		call(unaryexpr);
		doit("unaryincdec");

		return { true, currrecord };
	}

	switch (*currlexing) if (0);
	else if (0) {
	case '&':
	case '*':
	case '+':
	case '-':
	case '~':
	case '!':
		priormatches["unop"_h] = *currlexing;
		currlexing++;
		call(castexpr);
		doit("unary");

		return { true, currrecord };
	}

	const std::string sizeofkey = "sizeof";

	if (isonboundary() && 
		std::string{ currlexing, currlexing + sizeofkey.length() } == sizeofkey
		&& isonboundary(currlexing + sizeofkey.length())) {
		currlexing + sizeofkey.length();
		if (call(typename_)) {
			doit("endsizeoftypename");
		}
		else {
			doit("beginsizeofexpr");
			assert(call(unaryexpr));
			doit("endsizeofexpr");
		}

		return { true, currrecord };
	}

	return { false };
}

return_t unaryexpr(std::unordered_map<unsigned, std::string> priormatches, std::unordered_map<unsigned, unsigned> flag) {
	consumewhitespace();
	priormatches.clear();
	record currrecord;

	auto doit = [&](std::string what) {
		::doit(what, (void*)&priormatches, currrecord);
	};

	auto call = [&](return_t what(std::unordered_map<unsigned, std::string>, std::unordered_map<unsigned, unsigned> flag)
		) {
			return what(priormatches, flag).handle(priormatches, currrecord);
	};

	auto callfwd = [&](return_t what(std::unordered_map<unsigned, std::string>, std::unordered_map<unsigned, unsigned> flag)
		) {
			return what(priormatches, flag);
	};

	if (call(inner) || call(inparenths)) {
		while (call(postfix));
		return { true, currrecord };
	}
	else {
		return callfwd(unary);
	}
}

return_t castexpr(std::unordered_map<unsigned, std::string> priormatches, std::unordered_map<unsigned, unsigned> flag) {
	consumewhitespace();
	priormatches.clear();
	record currrecord;

	auto doit = [&](std::string what) {
		::doit(what, (void*)&priormatches, currrecord);
	};

	auto call = [&](return_t what(std::unordered_map<unsigned, std::string>, std::unordered_map<unsigned, unsigned> flag)
		) {
			return what(priormatches, flag).handle(priormatches, currrecord);
	};

	auto callfwd = [&](return_t what(std::unordered_map<unsigned, std::string>, std::unordered_map<unsigned, unsigned> flag)
		) {
			return what(priormatches, flag);
	};

	if (auto res = callfwd(typenamerev); res.result) {
		return res;
	}
	else {
		return callfwd(unary);
	}
}

return_t typenamerev(std::unordered_map<unsigned, std::string> priormatches, std::unordered_map<unsigned, unsigned> flag) {
	consumewhitespace();
	priormatches.clear();
	record currrecord;

	auto doit = [&](std::string what) {
		::doit(what, (void*)&priormatches, currrecord);
	};

	auto call = [&](return_t what(std::unordered_map<unsigned, std::string>, std::unordered_map<unsigned, unsigned> flag)
		) {
			return what(priormatches, flag).handle(priormatches, currrecord);
	};

	auto callfwd = [&](return_t what(std::unordered_map<unsigned, std::string>, std::unordered_map<unsigned, unsigned> flag)
		) {
			return what(priormatches, flag);
	};

	++recording;

	if (call(typename_)) {
		--recording;
		record tpnmrec = currrecord;
		call(castexpr);
		replay(tpnmrec);
		doit("applycast");
		return { true, currrecord };
	}

	--recording;

	return { false };
}