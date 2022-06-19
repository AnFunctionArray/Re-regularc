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

struct ctxprops {
	bool filtermatchesout = true;
	bool filterflagsout = true;
	bool filterrecordout = false;
};

struct ctx : ctxprops {

	bool result = true;
	record record;
	std::unordered_map<unsigned, std::string> matches;
	std::unordered_map<unsigned, unsigned> flags;

	bool call(auto what) {
		auto res = what(*this);

		if(res.result)
			*this = what(*this);

		return res.result;
	};

	void doit(std::string fnname) {

		if (recording) {
			docall(fnname.c_str(), fnname.length(), (void*)&matches);
		}
		else {
			record.push_back({ fnname.c_str(), matches });
		}
	}

	struct ctx& operator!() {
		result = false;
		return *this;
	}
};

struct return_t : ctx {
	return_t(const ctx& arg) : ctx{ arg } {
		if (filtermatchesout) {
			matches.clear();
		}
		if (filterflagsout) {
			flags.clear();
		}
		if (filterrecordout) {
			record.clear();
		}
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

return_t escape(ctx ctx) {

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

		ctx.matches["escaperaw"_h] = { begin, currlexing };

		ctx.doit("addescapesequencetostring");

		assert(currlexing != begin);

		return ctx;
	}

	return !ctx;
}

return_t stringlit(ctx ctx) {
	consumewhitespace();

	if (ranges::contains(std::array{ '\'', '\"' }, *currlexing)) {
		ctx.matches["begincharliteral"_h] = *currlexing++;
		auto lastnonescapebegin = currlexing;

		for (;;) {
			std::array potrawtext = { lastnonescapebegin, currlexing };

			auto checkrawcharcompletion = [&] {
				if (potrawtext[0] != potrawtext[1]) {
					ctx.matches["textraw"_h] = std::string{ potrawtext[0], potrawtext[1] };
					ctx.doit("addplaintexttostring");
				}
			};

			++recording;

			if (ctx.call(escape)) {
				lastnonescapebegin = currlexing;
				--recording;
				replay(ctx.record);
				checkrawcharcompletion();
			}
			replay(ctx.record);
			--recording;
			
			if (ctx.matches["begincharliteral"_h][0] == *currlexing) {
				checkrawcharcompletion();
				break;
			}
			else {
				currlexing++;
			}
		}
		doit("add_literal");
		consumewhitespace();
		return ctx;
	}

	return !ctx;
}

return_t numberliteral(ctx ctx) {
	consumewhitespace();
	
	switch (stringhash(std::string{ currlexing, currlexing + 1 }.c_str())) if (0);
	else if (0) {
	case "0x"_h:
		currlexing += 2;
		auto beg = currlexing;
		while (isxdigit(*currlexing++));
		ctx.matches["hex"_h] = std::string{ beg , currlexing };
	}
	else if (0) {
	case "0b"_h:
		currlexing += 2;
		auto beg = currlexing;
		while (ranges::contains(std::string{"01"}, *currlexing++));
		ctx.matches["bin"_h] = std::string{ beg , currlexing };
	}
	else if (0) {
	default:
		if (*currlexing == '0' && isdigit(currlexing[1])) {
			auto beg = currlexing;
			currlexing += 2;
			while (ranges::contains(std::string{ "01234567" }, *++currlexing));
			ctx.matches["oct"_h] = std::string{ beg , currlexing };
		}
		else if (isdigit(*currlexing)) {
			auto beg = currlexing;
			while (isdigit(*++currlexing));
			ctx.matches["dec"_h] = std::string{ beg , currlexing };
		}
		else {
			return !ctx;
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
			ctx.matches["lng"_h] = std::string{ beg , currlexing };
		}

		doit("num_lit");

		consumewhitespace();

		return !ctx;
	}
}

return_t exponent(ctx ctx) {

	(ctxprops)ctx = {false, true, false};

	if (ranges::contains(std::string{ "eE" }, *currlexing)) {
		auto beg = ++currlexing;
		if (ranges::contains(std::string{ "+-" }, *currlexing)) {
			ctx.matches["signexp"_h] = std::string{ beg, ++currlexing };
		}
		beg = currlexing;
		while (isdigit(*currlexing++));
		ctx.matches["exponent"_h] = std::string{ beg , currlexing };

		return ctx;
	}

	return !ctx;
}

return_t floating(ctx ctx) {
	consumewhitespace();

	if(*currlexing == '.') 
fraction:
	{
		auto beg = ++currlexing;
		while (isdigit(*currlexing++));
		ctx.matches["fraction"_h] = std::string{ beg , currlexing };
		ctx.call(exponent);
		goto success;
	}
	else if (isdigit(*currlexing)) {
		auto beg = currlexing;
		while (isdigit(*++currlexing));
		ctx.matches["whole"_h] = std::string{ beg , currlexing };
		if (*currlexing == '.') {
			goto fraction;
		}
		else if(ctx.call(exponent)) {
			goto success;
		}
	}
	else if (std::string{ currlexing, currlexing + 3 } == "NAN") {
		ctx.matches["nan"_h] = std::string{ currlexing, currlexing + 3 };
		currlexing += 3;
		goto success;
	}
	return !ctx;
success:
	{
		auto beg = currlexing;

		if (ranges::contains(std::string{ "flFL" }, *currlexing)) ++currlexing;

		if (beg != currlexing) {
			ctx.matches["suffixflt"_h] = std::string{ beg , currlexing };
		}

		ctx.doit("collect_float_literal");

		consumewhitespace();

		return ctx;
	}
}

bool isonboundary(std::string::iterator what = currlexing) {
	return (isalnum(what[-1]) || what[-1] == '_') != (isalnum(what[0]) || what[0] == '_');
}

bool isonboundary(std::string what) {
	if (isonboundary() &&
		std::string{ currlexing , currlexing + what.length() } == what &&
		isonboundary(currlexing + what.length())) {
		currlexing += what.length();
		return true;
	}
	return false;
}

return_t identifier(ctx ctx) {
	consumewhitespace();

	if (isonboundary() && (isalpha(*currlexing) || *currlexing == '_')) {
		auto beg = currlexing;
		++currlexing;
		while (isalnum(*currlexing) || *currlexing == '_') ++currlexing;
		ctx.matches["ident"_h] = std::string{ beg , currlexing };
		consumewhitespace();

		return ctx;
	}

	return !ctx;
}

return_t qualifiersidentifier(ctx ctx) {
	consumewhitespace();

	(ctxprops)ctx = { false, true, false };

	auto last = currlexing;

	if (ctx.call(identifier)) {
		switch (stringhash(ctx.matches["ident"_h].c_str())) if (0);
		else if (0) {
		case "const"_h:
		case "restrict"_h:
		case "volatile"_h:
		case "extern"_h:
		case "auto"_h:
		case "static"_h:
		case "register"_h:
			ctx.matches["qualiffound"_h] = std::move(ctx.matches["ident"_h]);
			ctx.doit("add_qualif");
			consumewhitespace();

			return ctx;
		}
	}

	currlexing = last;

	return !ctx;
}

return_t qualifiersortypeidentifier(ctx ctx) {
	consumewhitespace();

	(ctxprops)ctx = { false, true, false };

	auto last = currlexing;

	if (ctx.call(identifier)) {
		switch (stringhash(ctx.matches["ident"_h].c_str())) if (0);
		else if (0) {
		case "const"_h:
		case "restrict"_h:
		case "volatile"_h:
		case "extern"_h:
		case "auto"_h:
		case "static"_h:
		case "register"_h:
			ctx.matches["qualiffound"_h] = std::move(ctx.matches["ident"_h]);
			doit("add_qualif");
			consumewhitespace();

			return ctx;
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
			ctx.matches["typefound"_h] = std::move(ctx.matches["ident"_h]);
			ctx.doit("add_type");
			consumewhitespace();

			return ctx;
		}
	}

	currlexing = last;

	return !ctx;
}

return_t Tinside(ctx ctx);
return_t abstrsubs(ctx ctx);

return_t abstrptrrev(ctx ctx) {
	consumewhitespace();

	if (*currlexing == '*') {
		currlexing++;
		consumewhitespace();
		if (isonboundary("__stdcall")) {
			ctx.matches["callconv"_h] = "__stdcall";

			doit("setcallconv");

			consumewhitespace();
		}

		size_t currrecordsize = ctx.record.size();

		++recording;

		while (ctx.call(qualifiersidentifier));

		--recording;

		ctx.record.resize(currrecordsize);

		if (ctx.call(abstrptrrev)) {
			ctx.doit("addptrtotype");
		}
		else {
			assert(ctx.call(Tinside));
			while (ctx.call(abstrsubs));
			ctx.doit("addptrtotype");
		}
	}
}

return_t Tabstrrestalt(ctx ctx) {
	
	switch (ctx.flags["outter"_h]) if (0);
	else if (0) {
	case "params"_h:
		++recording;
		ctx.doit("identifier_decl");
		if (ctx.call(abstrsubs)) {
			--recording;
			replay(ctx.record);
			while (ctx.call(abstrsubs));
			return ctx;
		}
		ctx.record.pop_back();
		--recording;
		if (!ctx.call(Tinside)) {
			return !ctx;
		}

		while (ctx.call(abstrsubs));
		return ctx;
	}
	else if (0) {
	case "normal"_h:
		if (!ctx.call(Tinside)) {
			return !ctx;
		}

		while (ctx.call(abstrsubs));
		return ctx;
	}
	else if (0) {
	case "optional"_h:
		if (!ctx.call(Tinside)) {
			if (ctx.call(abstrsubs)) while (ctx.call(abstrsubs));
			else return !ctx;
			return ctx;
		}

		while (ctx.call(abstrsubs));
		return ctx;
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

return_t identifierminedecl(ctx ctx) {
	consumewhitespace();

	auto last = currlexing;

	if (ctx.call(identifier) && !isidentkeyword(ctx.matches["ident"_h])) {
		doit("identifier_decl");
		consumewhitespace();

		if (ctx.flags["optinit"_h] != "bitfl"_h) {
			identifiersmap.back()[stringhash(ctx.matches["ident"_h].c_str())] = ctx.matches.contains("typedefkey"_h);
		}

		return ctx;
	}

	currlexing = last;

	return !ctx;
}

return_t ident(ctx ctx) {
	consumewhitespace();

	auto last = currlexing;

	if (ctx.call(identifier) && !isidentkeyword(ctx.matches["ident"_h])) {
		ctx.doit("obtainvalbyidentifier");
		consumewhitespace();

		return ctx;
	}

	currlexing = last;

	return !ctx;
}

return_t abstdecl(ctx ctx) {
	consumewhitespace();

	if (isonboundary("__stdcall")) {

		ctx.matches["callconv"_h] = "__stdcall";

		ctx.doit("setcallconv");

		consumewhitespace();
	}

	if (!ctx.call(abstrptrrev)) {
		ctx.call(Tabstrrestalt);

		return ctx;
	}

	return ctx;
}


return_t Tinside(ctx ctx) {

	record currrecord;

	switch (ctx.flags["outter"_h]) if (0);
	else if (0) {
	case "params"_h:
		if (*currlexing == '(') {
			ctx.call(abstdecl);
			assert(*currlexing == ')');
			++currlexing;
			consumewhitespace();
		}
		else {
			if (!ctx.call(identifierminedecl)) {
				ctx.doit("identifier_decl");
			}
		}

		return ctx;
	}
	else if (0) {
	case "normal"_h:
		if (*currlexing == '(') {
			ctx.call(abstdecl);
			assert(*currlexing == ')');
			++currlexing;
			consumewhitespace();
			return ctx;
		}
		else {
			ctx.call(identifierminedecl);
			return ctx;
		}
	}
	else if (0) {
	case "optional"_h:
		if (*currlexing == '(') {
			ctx.call(abstdecl);
			assert(*currlexing == ')');
			++currlexing;
			consumewhitespace();
			return ctx;
		}
		
		return ctx;
	}

	assert(0);  "invocation without proper set flag!";
}

return_t primexpr(ctx ctx);
return_t primexprcall(ctx ctx);
return_t param(ctx ctx);

return_t abstrsubs(ctx ctx) {
	consumewhitespace();

	if (*currlexing == '[') {
		++currlexing;
		doit("beginconstantexpr");
		ctx.call(primexpr);
		assert(*currlexing == ']');
		++currlexing;
		ctx.doit("addsubtotype");
		ctx.doit("endconstantexpr");
		consumewhitespace();
		return ctx;
	}
	else if (*currlexing == '(') {
		++currlexing;
		consumewhitespace();
		ctx.doit("startfunctionparamdecl");

		if (std::string{ currlexing , currlexing + 3 } == "...") {
			ctx.matches["rest"_h] = std::string{ currlexing , currlexing + 3 };
			currlexing += 3;
			goto noparams;
		}
		else if (isonboundary("void")) {
			goto noparams;
		}

		while (ctx.call(param));

		if (std::string{ currlexing , currlexing + 3 } == "...") {
			ctx.matches["rest"_h] = std::string{ currlexing , currlexing + 3 };
			currlexing += 3;
		}

		noparams:
		{
			consumewhitespace();

			assert(*currlexing == ')');

			++currlexing;

			doit("endfunctionparamdecl");

			consumewhitespace();

			return ctx;
		}
	}

	return !ctx;
}

return_t identifier_typedef(ctx ctx) {
	consumewhitespace();

	(ctxprops)ctx = { false, true, false };

	auto last = currlexing;

	if (ctx.matches.contains("typedefnmmatched"_h)) {
		return !ctx;
	}

	if (ctx.call(identifier) && identifiersmap.back()[stringhash(ctx.matches["ident"_h].c_str())]) {
		consumewhitespace();

		ctx.matches["typedefnmmatched"_h] = std::move(ctx.matches["ident"_h]);

		return ctx;
	}

	return !ctx;
}

return_t typedefkey(ctx ctx) {
	consumewhitespace();

	(ctxprops)ctx = { false, true, false };

	if (isonboundary("typedef")) {

		ctx.matches["typedefkey"_h] = "typedef";

		consumewhitespace();

		return ctx;
	}

	return !ctx;
}

return_t assignorsomething(ctx ctx);

return_t enumerator(ctx ctx) {
	consumewhitespace();

	if (ctx.call(identifier)) {
		ctx.doit("add_ident_to_enum_def");
		if (*currlexing == '=') {
			ctx.doit("beginconstantexpr");
			ctx.call(assignorsomething);
			ctx.doit("end_ass_to_enum_def");
		}
		else {
			ctx.doit("end_without_ass_to_enum_def");
		}

		return ctx;
	}

	return !ctx;
}

return_t strcelem(ctx ctx) {
	ctx.matches.clear();

	ctx.flags["optinit"_h] = "bitfl"_h;
	ctx.flags["outter"_h] = "normal"_h;
	ctx.flags["opt"_h] = false;

	if (ctx.call(abstdeclorallqualifs)) {
		consumewhitespace();
		assert(*currlexing == ';');
		++currlexing;
		consumewhitespace();
		return ctx;
	}
	return !ctx;
}

return_t structorunion(ctx ctx) {
	consumewhitespace();

	(ctxprops)ctx = { false, true, false };

	bool isenum = false;

	if (isonboundary("union")) {
		ctx.matches["structorunionlast"_h] = "union";
	}
	else if (isonboundary("struct")) {
		ctx.matches["structorunionlast"_h] = "struct";
	}
	else if (isonboundary("enum")) {
		ctx.matches["enum"_h] = "enum";
		isenum = true;
	}

	consumewhitespace();

	auto last = currlexing;

	bool hastag = true;

	if (ctx.call(identifier) && !isidentkeyword(ctx.matches["ident"_h])) {
		consumewhitespace();
		ctx.matches["lasttag"_h] = std::move(ctx.matches["ident"_h]);
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
			if (ctx.call(enumerator)) {
				if (*currlexing == ',') {
					while (ctx.call(enumerator) && *currlexing == ',')
						++currlexing;
				}
			}

			assert(*currlexing == '}');

			currlexing++;

			consumewhitespace();

			recording = oldrecord;

			return ctx;
		}
		else {

			doit("struc_or_union_body");

			while (ctx.call(strcelem));

			assert(*currlexing == '}');

			doit("endbuildingstructorunion");

			currlexing++;

			consumewhitespace();

			recording = oldrecord;

			return ctx;
		}
	}

	if (!hastag) {

		return !ctx;
	}

	return ctx;
}

return_t abstdeclorallqualifsqualifs(ctx ctx) {
	record currrecord;

	if (!ctx.matches.contains("qualiffound"_h) && !ctx.matches.contains("typefound"_h)) {

		if (ctx.call(structorunion)) {
			return ctx;
		}
		else if (ctx.call(identifier_typedef)) {
			return ctx;
		}
	}

	if (ctx.call(typedefkey)) {
		return ctx;
	}

	return !ctx;
}

return_t abstrbitfield(ctx ctx) {
	consumewhitespace();

	if (*currlexing == ':') {
		++currlexing;
		consumewhitespace();
		ctx.doit("beginconstantexpr");
		ctx.call(assignorsomething);
		ctx.doit("endconstantexpr");

		return ctx;
	}

	return !ctx;
}

return_t designator(ctx ctx) {
	consumewhitespace();

	if (*currlexing == '[') {
		++currlexing;
		consumewhitespace();
		ctx.doit("beginconstantexpr");
		ctx.call(assignorsomething);
		ctx.doit("subobject");
		ctx.doit("endconstantexpr");

		assert(*currlexing == ']');
		++currlexing;
		consumewhitespace();
		return ctx;
	}
	else if (*currlexing == '.') {
		++currlexing;
		consumewhitespace();
		ctx.call(identifier);
		ctx.doit("submember");
		return ctx;
	}

	return !ctx;
}

return_t initializer(ctx ctx) {
	consumewhitespace();

	if (ctx.call(assignorsomething)) {
		ctx.doit("extract");
	}
	else if (*currlexing == '{') {
		++currlexing;
		consumewhitespace();
		if (ctx.call(designator)) {
			while (ctx.call(designator));
			assert(*currlexing == '=');
			++currlexing;
		}

		if (ctx.call(initializer)) {
			do {
				if (*currlexing != ',') break;
				currlexing++;
			} while (ctx.call(initializer));
		}

		assert(*currlexing == '}');

		++currlexing;

		consumewhitespace();

		return ctx;
	}

	return !ctx;
}

return_t abstrinitialization(ctx ctx) {
	consumewhitespace();

	if (*currlexing == '=') {
		++currlexing;
		consumewhitespace();
		ctx.doit("begin_initializer");
		ctx.call(initializer);
		ctx.doit("finalize_initializer");

		return ctx;
	}

	return !ctx;
}

return_t optinit(ctx ctx) {

	switch (ctx.flags["optinit"_h]) if (0);
	else if (0) {
	case "bifl"_h:
		ctx.call(abstrbitfield);
		return ctx;
	}
	else if (0) {
	case "init"_h:
		ctx.call(abstrinitialization);
		return ctx;
	}

	return !ctx;
}

return_t abstdeclorallqualifs(ctx ctx) {
	consumewhitespace();

	++recording;
	
	if (ctx.flags["opt"_h]) goto startwhile;

	if (ctx.call(abstdeclorallqualifsqualifs)) {
	startwhile:
		while (ctx.call(abstdeclorallqualifsqualifs));
	}
	else {
		--recording;
		return !ctx;
	}

	auto qualifrecord = ctx.record;

	ctx.record.clear();

	--recording;

	if (ctx.flags["outter"_h] == "optional"_h) {
		ctx.doit("identifier_decl");
		goto startdowhile;
	}

	if (ctx.call(abstdecl)) {
		do {
			ctx.doit("enddeclaration");
			replay(qualifrecord);
			ctx.doit("endqualifs");
			ctx.call(optinit);
			if (*currlexing != ',' || ctx.flags["outter"_h] != "normal"_h) break;
			++currlexing;
startdowhile:
		} while (ctx.call(abstdecl));

		return ctx;
	}
	else if (ctx.matches.contains("structorunionlast"_h)) {
		ctx.doit("check_stray_struc");
		return ctx;
	}
	else if (ctx.matches.contains("enum"_h)) {
		return ctx;
	}

	return !ctx;
}

return_t param(ctx ctx) {
	ctx.matches.clear();

	ctx.flags["outter"_h] = "params"_h;
	ctx.flags["opt"_h] = false;
	ctx.flags["optinit"_h] = "none"_h;

	ctx.call(abstdeclorallqualifs);
	return ctx;
}

return_t typename_(ctx ctx) {
	consumewhitespace();

	ctx.flags["outter"_h] = "optional"_h;
	ctx.flags["opt"_h] = false;
	ctx.flags["optinit"_h] = "none"_h;

	ctx.call(abstdeclorallqualifs);
	return ctx;
}

return_t inner(ctx ctx) {
	consumewhitespace();
	ctx.matches.clear();

	if(ctx.call(stringlit) || ctx.call(floating) || ctx.call(numberliteral) || ctx.call(numberliteral) || ctx.call(ident))

		return ctx;
	
	return !ctx;
}

return_t inparenths(ctx ctx) {
	consumewhitespace();

	if (*currlexing == '(') {
		++currlexing;
		ctx.call(primexpr);
		assert(*currlexing == ')');
		++currlexing;
		consumewhitespace();
		return ctx;
	}

	return !ctx;
}

return_t postfix(ctx ctx) {
	consumewhitespace();
	ctx.matches.clear();

	switch (*currlexing) if (0);
	else if (0) {
	case '[':
		ctx.call(primexpr);
		assert(*currlexing == ']');
		ctx.doit("subscript");
		++currlexing;
		consumewhitespace();
		return ctx;
	}
	else if (0) {
	case '(':
		ctx.doit("startfunctioncall");
		ctx.call(primexprcall);
		assert(*currlexing == ')');
		ctx.doit("endfunctioncall");
		++currlexing;
		consumewhitespace();
		return ctx;
	}

	if (*currlexing == '.') {
		ctx.matches["arrowordotraw"_h] = '.';
		++currlexing;
		goto memberaccess;
	}
	else if (std::string{ currlexing, currlexing + 2 } == "->") {
		ctx.matches["arrowordotraw"_h] = "->";
		currlexing += 2;
	memberaccess:
		ctx.call(identifier);
		ctx.doit("memberaccess");

		return ctx;
	}
	else if (ranges::contains(std::array{ "++"_h, "--"_h }, stringhash(std::string{ currlexing, currlexing + 2 }.c_str()))) {
		ctx.matches["postfixarithraw"_h] = std::string{ currlexing, currlexing + 2 };
		currlexing += 2;
		ctx.doit("unaryincdec");

		return ctx;
	}

	return !ctx;
}

return_t unary(ctx ctx) {
	consumewhitespace();
	ctx.matches.clear();

	if (ranges::contains(std::array{ "++"_h, "--"_h }, stringhash(std::string{ currlexing, currlexing + 2 }.c_str()))) {
		ctx.matches["prefixarithraw"_h] = std::string{ currlexing, currlexing + 2 };
		currlexing += 2;
		ctx.call(unaryexpr);
		ctx.doit("unaryincdec");

		return ctx;
	}

	switch (*currlexing) if (0);
	else if (0) {
	case '&':
	case '*':
	case '+':
	case '-':
	case '~':
	case '!':
		ctx.matches["unop"_h] = *currlexing;
		currlexing++;
		ctx.call(castexpr);
		ctx.doit("unary");

		return ctx;
	}

	if (isonboundary("sizeof")) {
		if (ctx.call(typename_)) {
			doit("endsizeoftypename");
		}
		else {
			doit("beginsizeofexpr");
			assert(ctx.call(unaryexpr));
			doit("endsizeofexpr");
		}

		return ctx;
	}

	return !ctx;
}

return_t unaryexpr(ctx ctx) {
	consumewhitespace();
	ctx.matches.clear();

	if (ctx.call(inner) || ctx.call(inparenths)) {
		while (ctx.call(postfix));
		return ctx;
	}
	else {
		return ctx.call(unary), ctx;
	}
}

return_t castexpr(ctx ctx) {
	consumewhitespace();
	ctx.matches.clear();

	if (ctx.call(typenamerev)) {
		return ctx;
	}
	else {
		return ctx.call(unary), ctx;
	}
}

return_t typenamerev(ctx ctx) {
	consumewhitespace();
	ctx.matches.clear();

	++recording;

	if (ctx.call(typename_)) {
		--recording;
		record tpnmrec = ctx.record;
		ctx.call(castexpr);
		replay(tpnmrec);
		ctx.doit("applycast");
		return ctx;
	}

	--recording;

	return !ctx;
}


return_t binopplusrest(ctx ctx) {

	(ctxprops)ctx = { false, false, false };

	if (ctx.matches.contains("addopraw"_h))
		goto mulopraw;
	else if(ctx.matches.contains("shiftopraw"_h))
		goto addopraw;
	else if (ctx.matches.contains("relopraw"_h))
		goto shiftopraw;
	else if (ctx.matches.contains("eqopraw"_h))
		goto relopraw;
	else if (ctx.matches.contains("andopraw"_h))
		goto eqopraw;
	else if (ctx.matches.contains("xoropraw"_h))
		goto andopraw;
	else if (ctx.matches.contains("oropraw"_h))
		goto xoropraw;
	else if (ctx.matches.contains("andlogicopraw"_h))
		goto oropraw;
	else if (ctx.matches.contains("orlogicopraw"_h))
		goto andlogicopraw;

orlogicopraw: 
	if (std::string{ currlexing, currlexing + 2 } == "||") {
		ctx.doit("begin_branch");
		ctx.flags["containbranch"_h] = true;
		ctx.matches["binoplast"_h] = ctx.matches["orlogicopraw"_h] = std::string{ currlexing, currlexing + 2 };

		currlexing += 2;

		consumewhitespace();

		return ctx;
	}
andlogicopraw:
	if (std::string{ currlexing, currlexing + 2 } == "&&") {
		ctx.doit("begin_branch");
		ctx.flags["containbranch"_h] = true;

		ctx.matches["binoplast"_h] = ctx.matches["andlogicopraw"_h] = std::string{ currlexing, currlexing + 2 };

		currlexing += 2;

		consumewhitespace();

		return ctx;
	}
oropraw:
	if (*currlexing == '|' && currlexing[1] != '|') {
		ctx.matches["binoplast"_h] = ctx.matches["oropraw"_h] = *currlexing++;

		consumewhitespace();

		return ctx;
	}
xoropraw:
	if (*currlexing == '^') {
		ctx.matches["binoplast"_h] = ctx.matches["binoplast"_h] = ctx.matches["xoropraw"_h] = *currlexing++;

		consumewhitespace();

		return ctx;
	}
andopraw:
	if (*currlexing == '&' && currlexing[1] != '&') {
		ctx.matches["binoplast"_h] = ctx.matches["andopraw"_h] = *currlexing++;

		consumewhitespace();

		return ctx;
	}
eqopraw:
	if (std::string{ currlexing, currlexing + 2 } == "==" || std::string{ currlexing, currlexing + 2 } == "!=") {
		ctx.matches["binoplast"_h] = ctx.matches["eqopraw"_h] = std::string{ currlexing, currlexing + 2 };

		currlexing += 2;

		consumewhitespace();

		return ctx;
	}
relopraw:
	if (*currlexing == '<' && currlexing[1] != '<' ||
		*currlexing == '>' && currlexing[1] != '>') {
		ctx.matches["binoplast"_h] = ctx.matches["relopraw"_h] = *currlexing++;

		consumewhitespace();

		return ctx;
	}
	if (std::string{ currlexing, currlexing + 2 } == "<=" || std::string{ currlexing, currlexing + 2 } == ">=") {
		ctx.matches["binoplast"_h] = ctx.matches["relopraw"_h] = std::string{ currlexing, currlexing + 2 };

		currlexing += 2;

		consumewhitespace();

		return ctx;
	}
shiftopraw:
	if (std::string{ currlexing, currlexing + 2 } == "<<" || std::string{ currlexing, currlexing + 2 } == ">>") {
		ctx.matches["binoplast"_h] = ctx.matches["shiftopraw"_h] = std::string{ currlexing, currlexing + 2 };

		currlexing += 2;

		consumewhitespace();

		return ctx;
	}
addopraw:
	if (ranges::contains(std::string{ "+-" }, *currlexing)) {
		ctx.matches["binoplast"_h] = ctx.matches["addopraw"_h] = *currlexing++;

		consumewhitespace();

		return ctx;
	}
mulopraw:
	if (ranges::contains(std::string{ "*/%" }, *currlexing)) {
		ctx.matches["binoplast"_h] = ctx.matches["mulopraw"_h] = *currlexing++;

		consumewhitespace();

		return ctx;
	}

	return !ctx;
}

return_t binopplusrest(ctx ctx) {
	(ctxprops)ctx = { false, false, false };

	if (ctx.call(binopplusrest)) {
		ctx.call(castexpr);
		while (ctx.call(binopplusrest));
		ctx.doit("binary");

		return ctx;
	}
	
	return !ctx;
}

return_t orlogiorsomething(ctx ctx) {
	(ctxprops)ctx = { false, false, false };

	if (ctx.call(binopplusrest)) {
		while (ctx.call(binopplusrest));
		return ctx;
	}
	return !ctx;
}

return_t ternaryrest(ctx ctx) {
	consumewhitespace();
	ctx.matches.clear();

	if (*currlexing == '?') {
		++currlexing;

		ctx.doit("begin_ternary");

		assert(ctx.call(primexpr));

		assert(*currlexing == ':');

		++currlexing;

		ctx.doit("mid_ternary");

		consumewhitespace();

		return ctx;
	}

	return !ctx;
}

return_t ternaryorsomething(ctx ctx) {
	ctx.matches.clear();

	auto lastrec = ctx.record;

	ctx.record.clear();

	++recording;

	if (ctx.call(ternaryrest)) {
		assert(ctx.call(castexpr));
		ctx.call(ternarylogicopt);

		--recording;

		ctx.record.splice(++ctx.record.begin(), lastrec);

		return ctx;
	}

	--recording;

	return !ctx;
}

return_t ternarylogicopt(ctx ctx) {
	ctx.call(orlogiorsomething);

	auto rec = ctx.record;
	--recording;
	ctx.record.clear();
	if (ctx.flags["containbranch"_h]) {
		ctx.doit("begin_binary");
	}
	replay(rec);
	if (ctx.flags["containbranch"_h]) {
		ctx.doit("end_binary");
	}

	ctx.call(ternaryorsomething);

	return ctx;
}

return_t assignorsomething(ctx ctx) {
	consumewhitespace();
	ctx.matches.clear();

	auto lastrec = ctx.record;

	ctx.record.clear();

	++recording;

	if (ctx.call(unaryexpr)) {
		if (*currlexing == '=' && currlexing[1] != '=') {
			ctx.matches["binoplast"_h] = '=';
			++currlexing;
			goto assignrest;
		}
		if (ranges::contains(std::array{ "*="_h, "/="_h, "%="_h, "+="_h, "-="_h, "<<="_h, ">>="_h, "&="_h, "^="_h, "|="_h },
			stringhash(std::string{ currlexing, currlexing + 2 }.c_str()))) {
			ctx.matches["binoplast"_h] = std::string{ currlexing, currlexing + 2 };
			currlexing += 2;
assignrest:
			--recording;
			ctx.call(assignorsomething);
			ctx.doit("binary");

			return ctx;
		}

		return ctx.call(ternarylogicopt), ctx;
	}
	else if (ctx.call(typenamerev)) {
		return ctx.call(ternarylogicopt), ctx;
	}

	--recording;

	ctx.record.splice(++ctx.record.begin(), lastrec);

	return !ctx;
}