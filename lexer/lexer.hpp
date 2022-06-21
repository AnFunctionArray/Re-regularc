#pragma once

extern std::string subject;

extern "C" void docall(const char* name, size_t szname, void* phashmap);extern size_t recording;

typedef std::list<std::pair<std::string, std::unordered_map<unsigned, std::string>>> record_t;

extern record_t records;

struct ctx_neg {};

void doit(std::string fnname, void* phashmap, record_t& refrecord);

static record_t extract_last_record(record_t::iterator start=records.end()) {
	if (start == records.end()) {
		start = --std::find_if(records.rbegin(), records.rend(), [&](auto& elem) {
			return elem.first.empty();
			}).base();
	}
	record_t ret;

	ret.splice(ret.end(), records, start, records.end());

	ret.pop_front();

	return ret;

}

struct ctx {

	bool result = true;
	std::unordered_map<unsigned, std::string> matches;
	std::unordered_map<unsigned, unsigned> flags;

	template<class argT>
	bool call(struct return_t_passthrough what(argT)) {
		auto res = what(*this);

		return res.result;
	}

	template<class argT>
	bool call(ctx what(argT)) {
		auto res = what(*this);

		if (res.result) {
			if (!res.matches.empty()) {
				matches.merge(res.matches);
			}
			if (!res.filterflagsout.empty()) {
				flags.merge(res.flags);
			}
		}

		return res.result;
	}

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
			records.push_back({ fnname.c_str(), matches });
		}
	}

	void replay(record_t &&what) {
		size_t i = 0;
		if (recording) {
			records.splice(records.end(), what);
		}
		else for (const auto& call : what) {
			::doit(call.first, (void*)&call.second, what);
			++i;
		}
	}

	struct ctx_neg operator!() {
		return {};
	}
};

typedef ctx& ctx_passthrough;

struct return_t_passthrough : ctx{
	return_t_passthrough(const ctx& in) {
		result = true;
	}
	return_t_passthrough(const struct ctx_neg& in) {
		result = false;
	}
};

return_t_passthrough cprogram(ctx_passthrough ctx);