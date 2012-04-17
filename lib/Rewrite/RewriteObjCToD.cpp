//===--- RewriteObjCToD.cpp - Playground for the code rewriter ---------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Hacks and fun related to the code rewriter.
//
//===----------------------------------------------------------------------===//

#include <string>
#include <vector>

#include <boost/call_traits.hpp>
#include <boost/foreach.hpp>
#include <boost/preprocessor/cat.hpp>
#include <boost/range.hpp>
#include <boost/typeof/typeof.hpp>
#include <boost/unordered_set.hpp>
#include <boost/utility/result_of.hpp>

#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/raw_ostream.h"

#include "clang/AST/ASTConsumer.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/Decl.h"
#include "clang/AST/DeclGroup.h"
#include "clang/AST/DeclObjC.h"
#include "clang/AST/DeclCXX.h"
#include "clang/Basic/Diagnostic.h"
#include "clang/Basic/LangOptions.h"
#include "clang/Basic/SourceLocation.h"
#include "clang/Basic/SourceManager.h"
#include "clang/Rewrite/ASTConsumers.h"
#include "clang/Rewrite/Rewriter.h"

#define auto BOOST_AUTO
#define foreach BOOST_FOREACH
#define param(T) boost::call_traits<T>::param_type
#define ref(T) boost::call_traits<T>::reference
#define const_ref(T) boost::call_traits<T>::const_reference
#define HashSet boost::unordered_set

#define uint unsigned int
#define byte char
#define ubyte unsigned char
#define null 0

const bool D2 = true;
const bool D1 = !D2;

// Range extensions for foreach
namespace boost
{
	template <> struct range_mutable_iterator<clang::LinkageSpecDecl> { typedef clang::DeclContext::decl_iterator type; };	
	template <> struct range_const_iterator<clang::LinkageSpecDecl> { typedef clang::DeclContext::decl_iterator type; };
	
	template <> struct range_mutable_iterator<clang::ObjCInterfaceDecl> { typedef clang::ObjCInterfaceDecl::protocol_iterator type; };
	template <> struct range_const_iterator<clang::ObjCInterfaceDecl> { typedef clang::ObjCInterfaceDecl::protocol_iterator type; };
	
	template <> struct range_mutable_iterator<clang::ObjCProtocolDecl> { typedef clang::ObjCProtocolDecl::protocol_iterator type; };
	template <> struct range_const_iterator<clang::ObjCProtocolDecl> { typedef clang::ObjCProtocolDecl::protocol_iterator type; };
	
	template <> struct range_mutable_iterator<clang::ObjCMethodDecl> { typedef clang::ObjCMethodDecl::param_iterator type; };
	template <> struct range_const_iterator<clang::ObjCMethodDecl> { typedef clang::ObjCMethodDecl::param_iterator type; };
	
	template <> struct range_mutable_iterator<clang::FunctionProtoType> { typedef clang::FunctionProtoType::arg_type_iterator type; };
	template <> struct range_const_iterator<clang::FunctionProtoType> { typedef clang::FunctionProtoType::arg_type_iterator type; };
	
	template <> struct range_mutable_iterator<clang::FunctionDecl> { typedef clang::FunctionDecl::param_iterator type; };
	template <> struct range_const_iterator<clang::FunctionDecl> { typedef clang::FunctionDecl::param_const_iterator type; };
}

namespace clang
{
	inline DeclContext::decl_iterator range_begin (ref(LinkageSpecDecl) a) { return a.decls_begin(); }
	inline DeclContext::decl_iterator range_begin (const_ref(LinkageSpecDecl) a) { return a.decls_begin(); }
	inline DeclContext::decl_iterator range_end (ref(LinkageSpecDecl) a) { return a.decls_end(); }
	inline DeclContext::decl_iterator range_end (const_ref(LinkageSpecDecl) a) { return a.decls_end(); }
	
	inline ObjCInterfaceDecl::protocol_iterator range_begin (ref(ObjCInterfaceDecl) a) { return a.protocol_begin(); }
	inline ObjCInterfaceDecl::protocol_iterator range_begin (const_ref(ObjCInterfaceDecl) a) { return a.protocol_begin(); }
	inline ObjCInterfaceDecl::protocol_iterator range_end (ref(ObjCInterfaceDecl) a) { return a.protocol_end(); }
	inline ObjCInterfaceDecl::protocol_iterator range_end (const_ref(ObjCInterfaceDecl) a) { return a.protocol_end(); }

	inline ObjCProtocolDecl::protocol_iterator range_begin (ref(ObjCProtocolDecl) a) { return a.protocol_begin(); }
	inline ObjCProtocolDecl::protocol_iterator range_begin (const_ref(ObjCProtocolDecl) a) { return a.protocol_begin(); }
	inline ObjCProtocolDecl::protocol_iterator range_end (ref(ObjCProtocolDecl) a) { return a.protocol_end(); }
	inline ObjCProtocolDecl::protocol_iterator range_end (const_ref(ObjCProtocolDecl) a) { return a.protocol_end(); }
	
	inline ObjCMethodDecl::param_iterator range_begin (ref(ObjCMethodDecl) a) { return a.param_begin(); }
	inline ObjCMethodDecl::param_iterator range_begin (const_ref(ObjCMethodDecl) a) { return a.param_begin(); }
	inline ObjCMethodDecl::param_iterator range_end (ref(ObjCMethodDecl) a) { return a.param_end(); }
	inline ObjCMethodDecl::param_iterator range_end (const_ref(ObjCMethodDecl) a) { return a.param_end(); }
	
	inline FunctionProtoType::arg_type_iterator range_begin (ref(FunctionProtoType) a) { return a.arg_type_begin(); }
	inline FunctionProtoType::arg_type_iterator range_begin (const_ref(FunctionProtoType) a) { return a.arg_type_begin(); }
	inline FunctionProtoType::arg_type_iterator range_end (ref(FunctionProtoType) a) { return a.arg_type_end(); }
	inline FunctionProtoType::arg_type_iterator range_end (const_ref(FunctionProtoType) a) { return a.arg_type_end(); }
	
	inline FunctionDecl::param_iterator range_begin (ref(FunctionDecl) a) { return a.param_begin(); }
	inline FunctionDecl::param_const_iterator range_begin (const_ref(FunctionDecl) a) { return a.param_begin(); }
	inline FunctionDecl::param_iterator range_end (ref(FunctionDecl) a) { return a.param_end(); }
	inline FunctionDecl::param_const_iterator range_end (const_ref(FunctionDecl) a) { return a.param_end(); }	
}

using namespace std;
using namespace llvm;
using namespace clang;

bool isDKeyword (string str);
string replace (string str, char c, string replacement);
string rewriteIdentifier (string str);
string rewriteSelector (string str, bool fullName = false);
string rewriteType (string str);
string rewriteType (const_ref(QualType) type, bool rewriteIdToObject = true);
string rewriteFunctionPointerType (const_ref(QualType) type);
string rewriteObjCObjectPointerType (const_ref(QualType) type);

struct NewLine {};
NewLine nl;

template<typename IteratorType, typename T, typename Begin, typename End> struct Iterator
{
	T t;
	Begin b;
	End e;
	typedef IteratorType iterator;
	typedef IteratorType const_iterator;	

	IteratorType begin ()
	{
		return (t->*b)();
	}

 	IteratorType end ()
	{
		return (t->*e)();
	}
};

#define iterator(iter, var, name) \
	typedef __typeof__(*var) __##iter##_##var##_##name##_Type__; \
	Iterator<__typeof__(var->BOOST_PP_CAT(name, _begin())), __typeof__(var), __typeof__(&__##iter##_##var##_##name##_Type__::BOOST_PP_CAT(name, _begin)), __typeof__(&__##iter##_##var##_##name##_Type__::BOOST_PP_CAT(name, _end))> iter = { var, &__##iter##_##var##_##name##_Type__::BOOST_PP_CAT(name, _begin), &__##iter##_##var##_##name##_Type__::BOOST_PP_CAT(name, _end) }

struct StringWrapper
{
	string str;
	
private:
	ubyte indendationLevel;
	bool shouldIndent;
		
public:
	
	StringWrapper() : str(), indendationLevel(0), shouldIndent(false) {}
	
	ref(StringWrapper) operator << (StringRef str)
	{
		if (shouldIndent)
		{
			indent();
			shouldIndent = false;
		}
		
		this->str += str;
		return *this;
	}
	
	ref(StringWrapper) operator << (NewLine)
	{			
		str += '\n';
		shouldIndent = indendationLevel > 0;
			
		return *this;
	}
	
	ref(StringWrapper) operator += (StringRef str)
	{		
		return *this << str;
	}
	
	ref(StringWrapper) operator += (NewLine)
	{
		return *this << nl;
	}
	
	string stripNewLines (size_t number = 2)
	{			
		return str.substr(0, str.length() - number);
	}
	
	void indent (ubyte indendationLevel, void (^block)())
	{
		this->indendationLevel = indendationLevel;
		indent();
		block();
		this->indendationLevel = 0;
	}
	
	void indent (void (^block)())
	{
		indent(1, block);
	}
	
private:
	ref(StringWrapper) indent ()
	{
		for (auto(i, 0); i < indendationLevel; i++)
			str += '\t';
			
		return *this;
	}
};

struct Class : StringWrapper
{
	
private:
	HashSet<string> mangledMethods;
	
public:	
	string getMethodName (ObjCMethodDecl* method, string name = "")
	{
		auto(mangledName, mangle(method));
		auto(selector, method->getSelector().getAsString());

		if (mangledMethods.find(mangledName) == mangledMethods.end())
		{
			mangledMethods.insert(mangledName);
			name = name == "" ? selector : name;
			return rewriteSelector(name);
		}
		
		return rewriteSelector(selector, true);
	}
	
private:
	string mangle (ObjCMethodDecl* method, string name = "")
	{
		auto(selector, method->getSelector().getAsString());
		name = name == "" ? rewriteSelector(selector) : name;
		
		string mangledName = name;
		
		foreach (ParmVarDecl* paramDecl, *method)
			mangledName += '_' + rewriteType(paramDecl->getType());
			
		return mangledName;
	}
};

struct OutFile
{
	StringWrapper before;
	StringWrapper after;
	StringWrapper imports;
	StringWrapper functions;
	
	vector<Class> classes;
	vector<Class> interfaces;
	
	Class currentClass;
	Class currentInterface;
	
	raw_ostream* outputStream;
	
	void write ()
	{
		*this << before.str;
		*this << imports.str << nl;
		
		foreach (const_ref(Class) cls, classes)
			*this << cls.str;
			
		*this << currentClass.str;
		
		foreach (const_ref(Class) cls, interfaces)
			*this << cls.str;
			
		*this << currentInterface.str;
		*this << functions.str;
		*this << after.str;
		
		outputStream->flush();
	}
	
	ref(OutFile) operator << (StringRef str)
	{
		*outputStream << string(str);
		return *this;
	}
	
	ref(OutFile) operator << (NewLine)
	{
		*outputStream << "\n";
		return *this;
	}
	
	ref(OutFile) operator += (StringRef str)
	{
		return *this << str;
	}
	
	ref(OutFile) append (StringRef str)
	{
		return *this << str;
	}

	ref(OutFile) appendln (StringRef str)
	{
		append(str);
		return append("\n");
	}
};

class RewriteObjCToD : public ASTConsumer
{
private:	

	const_ref(LangOptions) languageOptions;
	ref(Diagnostic) diagnostic;
	SmallPtrSet<ObjCProtocolDecl*, 32>  protocolExpressionDeclarations;
	SourceManager* sourceManager;
	FileID mainFileID;
	Rewriter rewriter;
	ASTContext* context;
	
	SmallVector<ObjCImplementationDecl*, 8> classImplementation;
	SmallVector<ObjCCategoryImplDecl*, 8> categoryImplementation;
	
	SmallPtrSet<ObjCInterfaceDecl*, 8> objcForwardDeclarations;
	
	// ObjC string constant support.
    VarDecl* constantStringClassReference;
	
	SourceLocation lastAppendLocation;
	
	uint rewriteFailedDiag;
	uint tryFinallyContainsReturnDiag;
	
	string inFilename;
	string preamble;
	
	const char* mainFileStart;
	const char* mainFileEnd;
	
	bool silenceRewriteMacroWarning;
	bool isHeader;
	const bool dstepSyntax;
	
	OutFile outFile;
		
public:
	
	RewriteObjCToD (string inFile, raw_ostream* ostream,
					ref(Diagnostic) diagnostic, const_ref(LangOptions) languageOptions,
					bool silenceMacroWarn);
					
	~RewriteObjCToD() {}
	
	virtual void Initialize (ref(ASTContext) context);
	virtual void HandleTopLevelDecl (DeclGroupRef declarationGroup);
	virtual void HandleTranslationUnit (ref(ASTContext) astContext);
	
private:
	
	void handleTopLevelSingleDeclaration (Decl* declaration);
	void rewriteImplementations ();
	void rewriteInclude ();	
	void rewriteObjcToDProtocolMetaData (ObjCProtocolDecl* objcProtocolDeclaration, StringRef prefix, StringRef className, ref(string) result);
	bool shouldSkipDeclaration (Decl* declaration);
	void synthesizeMetaDataIntoBuffer (ref(string) result);
	
	void handleDeclarationInMainFile (Decl* declaration);
	void rewriteCategoryDeclaration (Decl* declaration);
	void rewriteForwardProtocolDeclaration (ObjCForwardProtocolDecl* declaration);
	void rewriteFunctionDeclaration (FunctionDecl* declaration);
	void rewriteInterfaceDeclaration (ObjCInterfaceDecl* declaration);	
	void rewriteProtocolDeclaration (ObjCProtocolDecl* declaration);
	
	template<typename T> vector<StringRef> collectInterfaces (T declaration);
	void rewriteProperty (ObjCPropertyDecl* declaration, ref(Class) current);
	void rewriteMethodDeclaration (ObjCMethodDecl* declaration, ref(Class) current, string name = "");
	void writeClass (StringRef name, StringRef superClassName, param(vector<StringRef>) interfaces, void (^block)());
	void writeInterface (StringRef name, param(vector<StringRef>) baseInterfaces, void (^block)());
	
	void classInterfaceHelper (param(vector<StringRef>) interfaces, ref(Class) current, void (^block)());
	
	// utility functions
	string getSetterName (ObjCPropertyDecl* declaration);
	void insertText (SourceLocation sourceLocation, StringRef str, bool insertAfter = true);
	bool isHeaderFile (const_ref(string) filename);
	vector<StringRef> splitString (StringRef str, char c);
	string toLower (string str);
};

ASTConsumer* clang::CreateObjCToDRewriter(const_ref(string) inFile, raw_ostream* os, ref(Diagnostic) diags, const_ref(LangOptions) langOptions, bool silenceRewriteMacroWarning)
{
	return new RewriteObjCToD (inFile, os, diags, langOptions, silenceRewriteMacroWarning);
}

RewriteObjCToD::RewriteObjCToD (string inFile, raw_ostream* ostream,
								ref(Diagnostic) diagnostic, const_ref(LangOptions) languageOptions,
								bool silenceMacroWarn) :
									languageOptions(languageOptions),
									diagnostic(diagnostic),
									inFilename(inFile),
									dstepSyntax(false)
{
	isHeader = isHeaderFile(inFile);
	rewriteFailedDiag = diagnostic.getCustomDiagID(Diagnostic::Warning, "rewriting sub-expression within a macro (may not be correct)");
	tryFinallyContainsReturnDiag = diagnostic.getCustomDiagID(Diagnostic::Warning, "rewriter doesn't support user-specified control flow semantics for @try/@finally (code may not execute properly)");
	outFile.outputStream = ostream;
}

void RewriteObjCToD::Initialize (ref(ASTContext) context)
{
	this->context = &context;
	sourceManager = &this->context->getSourceManager();
	
	// Get the ID and start/end of the main file.
	mainFileID = sourceManager->getMainFileID();
	const MemoryBuffer* mainBuffer = sourceManager->getBuffer(mainFileID);
	mainFileStart = mainBuffer->getBufferStart();
	mainFileEnd = mainBuffer->getBufferEnd();
	
	rewriter.setSourceMgr(this->context->getSourceManager(), this->context->getLangOptions());
	lastAppendLocation = sourceManager->getLocForStartOfFile(mainFileID);
}

void RewriteObjCToD::HandleTopLevelDecl (DeclGroupRef declarationGroup)
{	
	foreach (Decl* declaration, declarationGroup)
		if (!shouldSkipDeclaration(declaration))		
			handleTopLevelSingleDeclaration(declaration);
}

void RewriteObjCToD::HandleTranslationUnit (ref(ASTContext) astContext)
{
	if (diagnostic.hasErrorOccurred())
		return;
		
	rewriteInclude();
	
	if (!dstepSyntax)
		outFile.imports << nl << "extern (Objective-C):" << nl;
	
	// Here's a great place to add any extra declarations that may be needed.
	// Write out meta data for each @protocol(<expr>).
	for (auto(i, protocolExpressionDeclarations.begin()), e = protocolExpressionDeclarations.end(); i != e; i++)
		rewriteObjcToDProtocolMetaData(*i, "", "", preamble);
		
	insertText(sourceManager->getLocForStartOfFile(mainFileID), preamble, false);
	
	if (classImplementation.size() || categoryImplementation.size())
		rewriteImplementations();
	
	outFile.write();
}
	
void RewriteObjCToD::handleTopLevelSingleDeclaration (Decl* declaration)
{
	if (diagnostic.hasErrorOccurred())
		return;
		
	/* 
	 * Two cases: either the decl could be in the main file, or it could be in a
	 * #included file.  If the former, rewrite it now.  If the later, check to see
	 * if we rewrote the #include/#import.
	 */
	SourceLocation location = declaration->getLocation();
	location = sourceManager->getInstantiationLoc(location);
	
	// If this is for a builtin, ignore it.
	if (location.isInvalid())
		return;
		
	if (auto(decl, dyn_cast<FunctionDecl>(declaration)))
		rewriteFunctionDeclaration(decl);
		
	else if (auto(decl, dyn_cast<VarDecl>(declaration)))
	{
		// declared in <Foundation/NSString.h>
		if (decl->getName() == "_NSConstantStringClassReference")
		{
			constantStringClassReference = decl;
			return;
		}
	}
	
	else if (auto(decl, dyn_cast<ObjCInterfaceDecl>(declaration))) rewriteInterfaceDeclaration(decl);
	else if (auto(decl, dyn_cast<ObjCCategoryDecl>(declaration))) rewriteCategoryDeclaration(decl);
	else if (auto(decl, dyn_cast<ObjCProtocolDecl>(declaration))) rewriteProtocolDeclaration(decl);
	else if (auto(decl, dyn_cast<ObjCForwardProtocolDecl>(declaration))) rewriteForwardProtocolDeclaration(decl);
			
	else if (auto(decls, dyn_cast<LinkageSpecDecl>(declaration)))		
		foreach (Decl* d, *decls) // Recurse into linkage specifications
			handleTopLevelSingleDeclaration(d);
			
	// If we have a declaration in the main file, see if we should rewrite it.
	if (sourceManager->isFromMainFile(location))
		return handleDeclarationInMainFile(declaration);
}

void RewriteObjCToD::rewriteImplementations ()
{

}

/*
 * Hopefully this will eventually be replaced with something 
 * like "Include-What-You-Use": http://code.google.com/p/cppclean/
 */
void RewriteObjCToD::rewriteInclude ()
{
	auto(mainBuf, sourceManager->getBufferData(mainFileID));

	const char* mainBufStart = mainBuf.begin();
	const char* mainBufEnd = mainBuf.end();
	size_t importLen = strlen("import");
	size_t includeLen = strlen("include");

	// Loop over the whole file, looking for includes and imports.
	for (const char* bufPtr = mainBufStart; bufPtr < mainBufEnd; ++bufPtr)
	{
		if (*bufPtr == '#')
		{
			size_t length = 1;

			if (++bufPtr == mainBufEnd)
				return;

			while (*bufPtr == ' ' || *bufPtr == '\t')
			{
				++length;

				if (++bufPtr == mainBufEnd)
					return;
			}

			bool isImport = strncmp(bufPtr, "import", importLen) == 0;
			bool isInclude = strncmp(bufPtr, "include", includeLen) == 0;

			if (isImport || isInclude)
			{
				// replace #import or #include with import
				length += isImport ? importLen : includeLen;
				bufPtr += length;
				length = 1;

				while (*bufPtr == ' ' || *bufPtr == '\t')
				{
					length++;

					if (++bufPtr == mainBufEnd)
						return;
				}

				// start of include file add compensation for < or "
				const char* start = bufPtr + 1;

				while (*bufPtr != '>' && *bufPtr != '"')
					if (++bufPtr == mainBufEnd)
						return;

				// end of include file
				const char* end = bufPtr;

				// the whole include file without < > or " "
				StringRef str(start, end - start);

				// buffer that will contain the import declaration
				string buffer = "import ";

				// loop variables
				size_t inner = 0;
				size_t outer = 0;
				vector<StringRef> strings = splitString(str, '/');

				foreach (StringRef s, strings)
				{
					StringRef t = s;					
					inner = 0;

					// slice off the extension if any
					foreach (char c, s)
					{
						if (c == '.')
						{
							t = s.slice(0, inner);
							break;
						}

						inner++;
					}					

					if (outer < strings.size() - 1)
					{
						buffer += toLower(t);						
						buffer += '.';
					}

					else
						buffer += t;

					outer++;
				}

				buffer += ";\n";

				// restore the compensation for the < > or " "
				start--;
				end++;
				
				outFile.imports += buffer;
			}
		}
	}
}

void RewriteObjCToD::rewriteObjcToDProtocolMetaData (ObjCProtocolDecl* objcProtocolDeclaration, StringRef prefix, StringRef className, ref(string) result)
{
	
}

void RewriteObjCToD::handleDeclarationInMainFile (Decl* declaration)
{
	
}

bool RewriteObjCToD::shouldSkipDeclaration (Decl* declaration)
{	
	if (!declaration || declaration->getLocation().isInvalid())
		return true;
		
	auto(fileId, sourceManager->getFileID(declaration->getLocation()));
	
	if (sourceManager->getMainFileID() != fileId)
		return true;
		
	return false;
}

void RewriteObjCToD::synthesizeMetaDataIntoBuffer (ref(string) result)
{
	
}

void RewriteObjCToD::rewriteCategoryDeclaration (Decl* declaration)
{

}

void RewriteObjCToD::rewriteForwardProtocolDeclaration (ObjCForwardProtocolDecl* declaration)
{

}

void RewriteObjCToD::rewriteFunctionDeclaration (FunctionDecl* declaration)
{
	if (dstepSyntax)
	{

	}
	
	else
	{
		outFile.functions << rewriteType(declaration->getResultType()) << " ";

		string name = declaration->getNameInfo().getAsString();
		uint numberOfParams = declaration->param_size();

		outFile.functions << name << " (";

		if (numberOfParams > 0)
		{
			uint i = 0;

			foreach (ParmVarDecl* paramDecl, *declaration)
			{
				outFile.functions << rewriteType(paramDecl->getType());
				outFile.functions << " " << rewriteIdentifier(paramDecl->getNameAsString());

				if (i < numberOfParams - 1)
					outFile.functions += ", ";

				i++;
			}
		}

		if (declaration->isVariadic())
		{
			if (numberOfParams > 0)
				outFile.functions << ", ";

			outFile.functions << "...";
		}			

		outFile.functions << ");" << nl;
	}
}

void RewriteObjCToD::rewriteInterfaceDeclaration (ObjCInterfaceDecl* declaration)
{
	string superClassName = "";
	
	if (auto(superClass, declaration->getSuperClass()))
		superClassName = superClass->getName();

	writeClass(declaration->getName(), superClassName, collectInterfaces(declaration), ^ {
		iterator(propIter, declaration, prop);
		iterator(instmethIter, declaration, instmeth);
		iterator(classmethIter, declaration, classmeth);

		foreach (ObjCPropertyDecl* e, propIter) rewriteProperty(e, outFile.currentClass);
		foreach (ObjCMethodDecl* e, instmethIter) rewriteMethodDeclaration(e, outFile.currentClass);
		foreach (ObjCMethodDecl* e, classmethIter) rewriteMethodDeclaration(e, outFile.currentClass);
	});
}

void RewriteObjCToD::rewriteProtocolDeclaration (ObjCProtocolDecl* declaration)
{
	writeInterface(declaration->getName(), collectInterfaces(declaration), ^ {
		iterator(propIter, declaration, prop);
		iterator(instmethIter, declaration, instmeth);
		iterator(classmethIter, declaration, classmeth);

		foreach (ObjCPropertyDecl* e, propIter) rewriteProperty(e, outFile.currentInterface);
		foreach (ObjCMethodDecl* e, instmethIter) rewriteMethodDeclaration(e, outFile.currentInterface);
		foreach (ObjCMethodDecl* e, classmethIter) rewriteMethodDeclaration(e, outFile.currentInterface);
	});
}

template<typename T> vector<StringRef> RewriteObjCToD::collectInterfaces (T declaration)
{
	vector<StringRef> interfaces;

	foreach (ObjCProtocolDecl* e, *declaration)
		interfaces.push_back(e->getName());
		
	return interfaces;
}

void RewriteObjCToD::rewriteProperty (ObjCPropertyDecl* declaration, ref(Class) current)
{
	auto(getter, declaration->getGetterMethodDecl());
	getter->setSynthesized(false);	

	auto(setter, declaration->getSetterMethodDecl());
	setter->setSynthesized(false);

	if (D2) current << "@property ";	
	rewriteMethodDeclaration(getter, current);

	if (D2) current << "@property ";	
	rewriteMethodDeclaration(setter, current, getSetterName(declaration));

	getter->setSynthesized(true);
	setter->setSynthesized(true);
}

void RewriteObjCToD::rewriteMethodDeclaration (ObjCMethodDecl* method, ref(Class) current, string name)
{
	// these are handled in rewriteProperty
	if (method->isSynthesized())
		return;

	if (dstepSyntax)
	{
		string code;
		string implCode;
		string args;

		if (!method->getResultType()->isVoidType())
			implCode += "return ";

		if (method->isClassMethod())
		{
			code += "static ";
			implCode += "invokeObjcSuperClass!(";
		}

		else
			implCode += "invokeObjcSelf!(";		

		string resultType = rewriteType(method->getResultType());
		code += resultType;
		code += ' ';

		implCode += resultType;
	
		string selector = method->getSelector().getAsString();
		implCode += ", \"" + selector + "\"";

		string methodName = name == "" ? selector : name;
		methodName = rewriteSelector(methodName);
		code += methodName + " (";
		uint numberOfParams = method->param_size();

		if (numberOfParams > 0)
		{
			uint i = 0;

			foreach (ParmVarDecl* paramDecl, *method)
			{
				string paramType = rewriteType(paramDecl->getType());
				code += paramType;
				code += " " + rewriteIdentifier(paramDecl->getNameAsString());
				implCode += ", " + paramType;
				args += rewriteIdentifier(paramDecl->getNameAsString());

				if (i < numberOfParams - 1)
				{
					code += ", ";
					args += ", ";
				}

				i++;
			}

			if (method->isVariadic())
			{
				code += "...";
				implCode += "...";
			}

			implCode += ")(" + args + ");\n";
		}	

		else	
			implCode += ");\n";

		code += ")\n{\n\t" + implCode + "}\n";

		current << code;
	}

	else
	{
		if (method->getImplementationControl() == ObjCMethodDecl::Optional)
			current << "@optional ";

		if (method->isClassMethod())
			current << "static ";
		
		current << rewriteType(method->getResultType()) << " ";
	
		string selector = method->getSelector().getAsString();
		string methodName = name == "" ? selector : name;
		methodName = current.getMethodName(method, methodName);
		uint numberOfParams = method->param_size();
	
		current << methodName << " (";
	
		if (numberOfParams > 0)
		{
			uint i = 0;

			foreach (ParmVarDecl* paramDecl, *method)
			{
				current << rewriteType(paramDecl->getType());
				current << " " << rewriteIdentifier(paramDecl->getNameAsString());

				if (i < numberOfParams - 1)
					current += ", ";

				i++;
			}
		}
	
		if (method->isVariadic())
		{
			if (numberOfParams > 0)
				current << ", ";
			
			current << "...";
		}			
		
		current << ") [" << selector << "];" << nl;
	}
}

void RewriteObjCToD::writeClass (StringRef name, StringRef superClassName, param(vector<StringRef>) interfaces, void (^block)())
{
	outFile.classes.push_back(outFile.currentClass);
	outFile.currentClass = Class();	
	outFile.currentClass << "class " << name;
	
	if (superClassName != "")
		outFile.currentClass << " : " << superClassName;
		
	classInterfaceHelper(interfaces, outFile.currentClass, block);
}

void RewriteObjCToD::writeInterface (StringRef name, param(vector<StringRef>) baseInterfaces, void (^block)())
{
	outFile.interfaces.push_back(outFile.currentInterface);
	outFile.currentInterface = Class();	
	outFile.currentInterface << "interface " << name;
	classInterfaceHelper(baseInterfaces, outFile.currentInterface, block);
}

void RewriteObjCToD::classInterfaceHelper (param(vector<StringRef>) interfaces, ref(Class) current, void (^block)())
{
	if (!interfaces.empty())	
		current << " : ";

	size_t i = 0;

	foreach (StringRef inter, interfaces)
	{
		current << inter;

		if (i != interfaces.size() - 1)
			current << ", ";

		i++;
	}

	current << nl;
	current << "{" << nl;
	current.indent(^{ block(); });
	current << "}" << nl << nl;
}

string replace (string str, char match, char replacement)
{
	string s = str;
	
	foreach (ref(char) c, s)
		if (c == match)
			c = replacement;
			
	return s;
}

string rewriteIdentifier (string str)
{
	if (isDKeyword(str))
		return str + '_';

	return str;
}

string rewriteSelector (string str, bool fullName)
{
	if (fullName)
		str = replace(str, ':', '_');
	
	else
	{
		auto(index, str.find(':'));

		if (index != string::npos)
			str = str.substr(0, index);
	}
		
	return rewriteIdentifier(str);
}

string rewriteType (string str)
{
	if (str == "long") return "c_long";
	if (str == "BOOL") return "bool";
	if (str == "unsigned" || str == "unsigned int") return "uint";

	return str;
}

string rewriteType (const_ref(QualType) type, bool rewriteIdToObject)
{
	if (type->isFunctionPointerType() || type->isBlockPointerType())
		return rewriteFunctionPointerType(type);

	if (type->isObjCObjectPointerType() && !type->isObjCBuiltinType())
		return rewriteObjCObjectPointerType(type);
	
	if (type->isPointerType() && !type->isObjCBuiltinType())
		return rewriteType(type->getPointeeType()) + "*";
		
	// if (const ArrayType* arrayType = type->getAs<ArrayType>())
	//	return rewriteType(arrayType->getElementType()) + "[]";	

	if (type->isBooleanType()) return "bool";
	if (type->isWideCharType()) return "wchar";	
	if (type->isObjCIdType()) return rewriteIdToObject ? "Object" : "id";
			

	return rewriteType(type.getAsString());
}

string rewriteFunctionPointerType (const_ref(QualType) type)
{
	if (type->isFunctionPointerType() || type->isBlockPointerType())
	{
		// If the function pointer is a typedef we just return that.
		if (type->getAs<TypedefType>())
			return type.getAsString();
		
		// needs special handling, since pointer-to-functions have special
		// syntax (where a declaration models use).
		QualType pointeeType;

		if (const PointerType* pt = type->getAs<PointerType>())
			pointeeType = pt->getPointeeType();

		else if (const BlockPointerType* bpt = type->getAs<BlockPointerType>())
			pointeeType = bpt->getPointeeType();

		if (const FunctionProtoType* fpType = pointeeType->getAs<FunctionProtoType>())
		{
			string code = rewriteType(fpType->getResultType(), false) + " function (";
			unsigned numberOfArgs = fpType->getNumArgs();
			size_t i = 0;
			
			foreach (const_ref(QualType) type, *fpType)
			{
				code += rewriteType(type, false);
				
				if (i < numberOfArgs - 1)
					code += ", ";
									
				i++;
			}
			
			if (fpType->isVariadic())
			{
				if (numberOfArgs > 0)
					code += ", ";

				code += "...";
			}				
				
			return code += ')';
		}
		
		return type.getAsString();
	}
	
	return rewriteType(type);
}

string rewriteObjCObjectPointerType (const_ref(QualType) type)
{
	if (type->isObjCObjectPointerType() && !type->isObjCBuiltinType())
	{
		QualType pointeeType = type->getPointeeType();

		// Protocol is a special case
		if (pointeeType.getAsString() == "Protocol")
			return "Protocol*";

		return rewriteType(pointeeType);
	}
	
	return rewriteType(type);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////

string RewriteObjCToD::getSetterName (ObjCPropertyDecl* declaration)
{
	string setterName = declaration->getSetterName().getAsString();
	
	if (setterName.substr(0, 3) == "set")
		setterName = setterName.substr(3);
		
	return toLower(setterName);
}

void RewriteObjCToD::insertText (SourceLocation sourceLocation, StringRef str, bool insertAfter)
{
	// If insertion succeeded or warning disabled return with no warning.
	if (!rewriter.InsertText(sourceLocation, str, insertAfter) || silenceRewriteMacroWarning)
		return;
		
	diagnostic.Report(context->getFullLoc(sourceLocation), rewriteFailedDiag);
}

bool isDKeyword (string str)
{
	if (str == "abstract") return true;
	else if (str == "alias") return true;
	else if (str == "align") return true;
	else if (str == "asm") return true;
	else if (str == "assert") return true;
	else if (str == "auto") return true;

	else if (str == "body") return true;
	else if (str == "bool") return true;
	else if (str == "break") return true;
	else if (str == "byte") return true;

	else if (str == "case") return true;
	else if (str == "cast") return true;
	else if (str == "catch") return true;
	else if (str == "cdouble") return true;
	else if (str == "cent") return true;
	else if (str == "cfloat") return true;
	else if (str == "char") return true;
	else if (str == "class") return true;
	else if (str == "const") return true;
	else if (str == "continue") return true;
	else if (str == "creal") return true;

	else if (str == "dchar") return true;
	else if (str == "debug") return true;
	else if (str == "default") return true;
	else if (str == "delegate") return true;
	else if (str == "delete") return true;
	else if (str == "deprecated") return true;
	else if (str == "do") return true;
	else if (str == "double") return true;

	else if (str == "else") return true;
	else if (str == "enum") return true;
	else if (str == "export") return true;
	else if (str == "extern") return true;

	else if (str == "false") return true;
	else if (str == "final") return true;
	else if (str == "finally") return true;
	else if (str == "float") return true;
	else if (str == "for") return true;
	else if (str == "foreach") return true;
	else if (str == "foreach_reverse") return true;
	else if (str == "function") return true;

	else if (str == "goto") return true;

	else if (str == "idouble") return true;
	else if (str == "if") return true;
	else if (str == "ifloat") return true;
	else if (str == "import") return true;
	else if (str == "in") return true;
	else if (str == "inout") return true;
	else if (str == "int") return true;
	else if (str == "interface") return true;
	else if (str == "invariant") return true;
	else if (str == "ireal") return true;
	else if (str == "is") return true;

	else if (str == "lazy") return true;
	else if (str == "long") return true;

	else if (str == "macro") return true;
	else if (str == "mixin") return true;
	else if (str == "module") return true;

	else if (str == "new") return true;
	else if (str == "nothrow") return true;
	else if (str == "null") return true;

	else if (str == "out") return true;
	else if (str == "override") return true;

	else if (str == "package") return true;
	else if (str == "pragma") return true;
	else if (str == "private") return true;
	else if (str == "protected") return true;
	else if (str == "public") return true;
	else if (str == "pure") return true;

	else if (str == "real") return true;
	else if (str == "ref") return true;
	else if (str == "return") return true;

	else if (str == "scope") return true;
	else if (str == "shared") return true;
	else if (str == "short") return true;
	else if (str == "static") return true;
	else if (str == "struct") return true;
	else if (str == "super") return true;
	else if (str == "switch") return true;
	else if (str == "synchronized") return true;

	else if (str == "template") return true;
	else if (str == "this") return true;
	else if (str == "throw") return true;
	else if (str == "true") return true;
	else if (str == "try") return true;
	else if (str == "typedef") return true;
	else if (str == "typeid") return true;
	else if (str == "typeof") return true;

	else if (str == "ubyte") return true;
	else if (str == "ucent") return true;
	else if (str == "uint") return true;
	else if (str == "ulong") return true;
	else if (str == "union") return true;
	else if (str == "unittest") return true;
	else if (str == "ushort") return true;

	else if (str == "version") return true;
	else if (str == "void") return true;
	else if (str == "volatile") return true;

	else if (str == "wchar") return true;
	else if (str == "while") return true;
	else if (str == "with") return true;
	
	else if (str == "__FILE__") return true;
	else if (str == "__LINE__") return true;
	else if (str == "__DATE__") return true;
	else if (str == "__TIME__") return true;
	else if (str == "__TIMESTAMP__") return true;
	else if (str == "__VENDOR__") return true;
	else if (str == "__VERSION__") return true;
	
	else if (D2)
	{
		if (str == "immutable") return true;
		if (str == "nothrow") return true;
		if (str == "pure") return true;
		if (str == "shared") return true;

		if (str.size() > 1 && str[0] == '@') return true;

		if (str == "__gshared") return true;
		if (str == "__thread") return true;
		if (str == "__traits") return true;
		
		if (str == "__EOF__") return true;
		
		else return false;	
	}
	
	else return false;
}

bool RewriteObjCToD::isHeaderFile (const_ref(string) filename)
{
	string::size_type dotPosition = filename.rfind('.');
	
	if (dotPosition == string::npos)
		return false;
		
	string extension = string(filename.begin() + dotPosition + 1, filename.end());
	
	return extension == "h" || extension == "hh" || extension == "H";
}

vector<StringRef> RewriteObjCToD::splitString (StringRef str, char c)
{
	vector<StringRef> list;
	size_t i = 0;
	size_t start = 0;
	
	foreach (char a, str)
	{
		if (a == c)
		{
			list.push_back(str.slice(start, i));
			start = i + 1;
		}
			
		i++;
	}
	
	if (!list.empty())
		list.push_back(str.slice(start, i));
	
	return list;
}

string RewriteObjCToD::toLower (string str)
{
	foreach (ref(char) c, str)
		if (c >= 'A' && c <= 'Z')
			c = c - 'A' + 'a';
		
	return str;
}
