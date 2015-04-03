///
/// \file
/// \ingroup support
/// \brief Basic utility header
///
#ifndef _BASIC_UTILITIES_
#define _BASIC_UTILITIES_
#include <iostream>
#include <string>
#include <fstream>
#include "PrimitiveTypes.H"


void Vectorize(std::vector<std::string> &retVal,const char **in);
void Vectorize(std::vector<std::string> &retVal,const char **in,int);
int String2Buf(const std::string &instr,void **buf);

void GetContentUntil(std::istream &In,
		     std::string ret,
		     const std::string &etag);

std::string GetNextContent(std::istream &In);


// must be sorted, or this doesn't work out
template<typename Container>
bool LessThan(const Container &c1,const Container &c2)
{
  typename Container::const_iterator c1i = c1.begin();
  typename Container::const_iterator c2i = c2.begin();
  while(c1i != c1.end() && c2i != c2.end()){
    if(*c1i < *c2i)
      return(true);
    if(*c1i++ > *c2i++)
      return(false);
  }
  if(c1i == c1.end())
    if(c2i == c2.end())
      return(false);
    else
      return(true);
  return(false);
}


// Inverts a container
template<typename Container>
void InvertContainer(const Container &c1,Container &c2)
{
  unsigned int size = c1.size();
  c2.resize(size);
  typename Container::const_iterator c1i = c1.begin();
  for(unsigned int iii = 0;iii < size;iii++)
    c2[*c1i++] = iii;
} 

///
/// \brief Dump container contents
///
/// Dumps the contents of the container to the specified std::ostream 
/// object.
///
template<typename ContainerType>
void DumpContents(std::ostream &Ostr,const ContainerType &c,std::string del = "\n"){
  if(!c.empty()){
    typename ContainerType::const_iterator ci = c.begin();
    Ostr << *ci++;
    while(ci != c.end())
      Ostr << del << *ci++;
  }
}

template<typename IndexType,typename ValueType>
void Invert(IndexedVectorObj<IndexType,ValueType> &ico){
  return;
}


template<typename ContainerType,typename T>
void CopyIntoContainer(ContainerType &cont,const T* src,size_t count)
{
  size_t datasize = sizeof(T);
  cont.resize(count);
  memcpy(&cont[0],src,count*datasize);
}

template<typename OuterCont,typename OutCont,typename InnerCont,typename MapType>
void MapElements(OuterCont &src,OutCont &trg,MapType &m)
{
  trg.resize(src.size());
  typename OuterCont::iterator sci = src.begin();
  typename OutCont::iterator oci = trg.begin();
  while(sci != src.end()){
    typename InnerCont::iterator ici = sci->begin();
    while(ici != sci->end())
      oci->push_back(m[*ici++]);
    sci++;
    oci++;
  }
}

/// Return the maximum of all elements of a multicontainer
template<typename ListContainerType, typename ListType>
Primitive::IndexType MaxNodeId(const ListContainerType &fc)
{
  Primitive::IndexType maxid = 0;
  typename ListContainerType::const_iterator lci = fc.begin();
  while(lci != fc.end()){
    typename ListType::const_iterator li = lci->begin();
    while(li != lci->end()){
      if(maxid < *li)
	maxid = *li;
      li++;
    }
  }
  return(maxid);
}

/// Populate OutCont with a flat list of entries from a multicontainer
template<typename OuterCont,typename InnerCont,typename OutCont>
void Flatten(OuterCont &con,OutCont &ocon){
  typename OuterCont::iterator cci = con.begin();
  ocon.resize(0);
  while(cci != con.end()){
    typename InnerCont::iterator ccni = cci->begin();
    while(ccni != cci->end())
      ocon.push_back(*ccni++);
    cci++;
  }
}

/// Return the total number of entries in a multicontainer 
template<typename OuterContType>
Primitive::IndexType GetTotalSize(OuterContType &con)
{
  Primitive::IndexType total_size = 0;
  typename OuterContType::iterator ci = con.begin();
  while(ci != con.end()){
    total_size += ci->size();
    ci++;
  }
  return(total_size);
}

template<typename OuterContType,typename InnerContType,typename RetCont,typename idxtype>
void MultiContainer2CSR(RetCont &xadj,RetCont &adj,
			OuterContType &source)
{
  Primitive::IndexType number_of_elements = source.size();
  xadj.resize(number_of_elements+1);
  Primitive::IndexType number_entries = GetTotalSize<OuterContType>(source);
  adj.reserve(number_entries);
  typename OuterContType::iterator si = source.begin();
  Primitive::IndexType elm = 0;
  xadj[elm++] = 0;
  while(si != source.end()){
    typename InnerContType::iterator ei = si->begin();
    xadj[elm] = static_cast<idxtype>(si->size() + xadj[elm-1]);
    elm++;
    while(ei != si->end())
      adj.push_back(static_cast<idxtype>(*ei++-1));
    si++;
  }
}
		     
/// Given an array of adjacent node lists (like an array
/// of face connectivities), this function will loop thru
/// and create a list of unique adjacent nodes for each node
/// with the nodes that *are actually adjacent in the lists*
template<typename ListContainerType, typename ListType>
void CreateAdjacentNodeList(std::vector<std::list<Primitive::IndexType> > &anodelist,
			    ListContainerType &fc,Primitive::IndexType nnodes=0)
{
  if(nnodes == 0) 
    nnodes = MaxNodeId<ListContainerType,ListType>(fc);
  anodelist.resize(nnodes);
  typename ListContainerType::iterator lci = fc.begin();
  while(lci != fc.end())
    {
      typename ListType::iterator li = lci->begin();
      while(li != lci->end()){
	Primitive::IndexType this_node = *li++ - 1;
	Primitive::IndexType next_node = 0;
	if(li == lci->end())
	  next_node = *(lci->begin());
	else
	  next_node = *li++;
	anodelist[this_node].push_back(next_node);
	anodelist[next_node-1].push_back(this_node+1);
      }
      lci++;
    }
  for(Primitive::IndexType node = 0;node < nnodes;node++)
    {
      anodelist[node].sort();
      anodelist[node].unique();
    }
}

/// Given an array of adjacent node lists (like an array
/// of face connectivities), this function will loop thru
/// and create a list of unique adjacent nodes for each node
/// Note that this is a little different from the AdjNodeList
/// because in this version *every node in each list* is considered
/// adjacent.
template<typename ListContainerType, typename ListType>
void AdjEList(std::vector<std::list<Primitive::IndexType> > &aelist,
	      ListContainerType &dual_con,unsigned long nel=0)
{
  if(nel == 0) 
    nel = MaxNodeId<ListContainerType,ListType>(dual_con);
  aelist.resize(nel);
  typename ListContainerType::iterator lci = dual_con.begin();
  while(lci != dual_con.end())
    {
      typename ListType::iterator li = lci->begin();
      while(li != lci->end()){
	Primitive::IndexType this_node = *li++ - 1;
	typename ListType::iterator li2 = li;
	while(li2 != lci->end()){
	  Primitive::IndexType nexnode = *li2++ - 1;
	  aelist[this_node].push_back(nexnode+1);
	  aelist[nexnode].push_back(this_node+1);
	}
      }
      lci++;
    }
  for(Primitive::IndexType node = 0;node < nel;node++)
    {
      aelist[node].sort();
      aelist[node].unique();
    }
}

template<typename ConType,typename IConType>
Primitive::IndexType NumberOfEdges(ConType &con)
{
  typename ConType::iterator ci = con.begin();
  Primitive::IndexType node = 0;
  Primitive::IndexType nedges = 0;
  while(ci != con.end())
    {
      Primitive::IndexType tnode = ++node;
      typename IConType::iterator eni = ci->begin();
      while(eni != ci->end())
	{
	  Primitive::IndexType anode = *eni++;
	  if((tnode) < anode)
	    nedges++;
	}
      ci++;
    }
  return(nedges);
}

template<typename ContainerType,typename Icont>
void FormGraph(const ContainerType &adjlist)
{
  // empty for now
}


///
/// \brief Cyclic test
///
/// Typically, this is used to determine if two representations of
/// the same face are oriented the same.
///
template<typename Container>
bool HaveSameOrientation(const Container &c1,const Container &c2)
{
  if(c1.size() != c2.size())
    return(false);
  typename Container::const_iterator c1i = c1.begin();
  typename Container::const_iterator c2i = 
    std::find(c2.begin(),c2.end(),*c1i);
  if(c2i == c2.end())
    return(false);
  while(c1i != c1.end()){
    if(*c1i != *c2i)
      return false;
    c1i++;
    c2i++;
    if(c2i == c2.end())
      c2i = c2.begin();
    
  }
  return(true);
}

///
/// \brief Anti-cyclic test
///
/// Typically, this is used to determine if two representations of
/// the same face are oriented the same.
///
template<typename Container>
bool HaveOppositeOrientation(const Container &c1,const Container &c2)
{
  if(c1.size() != c2.size())
    return(false);
  typename Container::const_iterator c1i = c1.begin();
  typename Container::const_reverse_iterator c2i = 
    std::find(c2.rbegin(),c2.rend(),*c1i);
  if(c2i == c2.rend())
    return(false);
  while(c1i != c1.end()){
    if(*c1i++ != *c2i++)
      return false;
    if(c2i == c2.rend())
      c2i = c2.rbegin();
  }
  return(true);
}

///
/// \brief Tokenize string
///
/// Breaks source string up into a vector of space delimited tokens
///
void TokenizeString(std::vector<std::string> &tokens,const std::string &source);

///
/// \brief Tokenize string w/ delimiter
///
/// Breaks source string up into a vector of tokens delimited by specified delimiter
///
void TokenizeString(std::vector<std::string> &tokens,const std::string &source,const char delim);

///
/// \brief File opener
///
int OpenFile(std::ifstream &Inf,const std::string &filename);
///
/// \brief Strip absolute path
/// 
/// Strips the absolute path to recover the base file name or
/// the executable file name.
///
const std::string stripdirs(const std::string &pname);

///
/// \brief Creates space delimited tokens in place
///
void Trim(std::string &instr,bool preserve_newline = false);

///
/// \brief Returns space delimited tokens
///
const std::string Trimmed(const std::string &instr,bool preserve_newline = false);

#endif
