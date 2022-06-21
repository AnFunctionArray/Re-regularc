#pragma once

extern std::string subject;

extern "C" void docall(const char* name, size_t szname, void* phashmap);extern size_t recording;

struct ctxprops {
	bool filtermatchesout = true;
	bool filterflagsout = true;
	bool filterrecordout = false;
};

typedef std::list<std::pair<std::string, std::unordered_map<unsigned, std::string>>> record_t;

struct ctx : ctxprops {

	bool result = true;
	record_t record;
	std::unordered_map<unsigned, std::string> matches;
	std::unordered_map<unsigned, unsigned> flags;

	template<typename argT>
	bool call(argT what) {
		auto res = what(*this);

		if (res.result) {
			if (!res.filtermatchesout) {
				matches = res.matches;
			}
			if (!res.filterflagsout) {
				flags = res.flags;
			}
			if (!res.filterrecordout) {
				record = res.record;
			}
		}

		return res.result;
	};

	void doit(std::string fnname) {
		auto matchescopy = matches;
		if (!recording) {
			docall(fnname.c_str(), fnname.length(), (void*)&matchescopy);
			std::cout << ">>" << fnname << std::endl;
			for (auto& m : matches) {
				std::cout << m.second << std::endl;
			}
		}
		else {
			record.push_back({ fnname.c_str(), matches });
		}
	}

	void replay(record_t what) {
		size_t i = 0;
		if (recording) {
			record.splice(record.end(), what);
		}
		else for (const auto& call : what) {
			::doit(call.first, (void*)&call.second, what);
			++i;
		}

		//what.clear();
	}

	struct ctx& operator!() {
		result = false;
		return *this;
	}
};

using return_t = ctx;

struct arg_clear_t : ctx {
	arg_clear_t(const ctx& arg) {
		flags = arg.flags;
		record = arg.record;
	}
};

return_t cprogram(arg_clear_t ctx);