/**
 *  @brief The PaneConnectivity object holds the inter-zone connectivity of each grid.
 *  @file PaneConnectivity.hpp
 *  @date Aug 13, 2009
 *  @author George Zagaris (gzagaris@illinois.edu)
 */

#ifndef PANECONNECTIVITY_HPP_
#define PANECONNECTIVITY_HPP_

#include <map>
#include <utility>
#include <vector>
#include <algorithm>
#include <cassert>

namespace Rocstar { namespace Rocin { namespace Utilities {

	class PaneConnectivity
	{
		private:
			std::map< int, std::vector< int > > 		pconn;									/*!< @brief Holds a mapping of remote pane to a list of local ids that are shared. 																					 */
			std::map< int, std::vector< int > >			point2remotePaneList;		/*!< @brief Holds a mapping of localpoint ids to remote pane ids 																	 */
			std::map< std::pair< int, int >, int >  rmtpanePointPair2Index;	/*!< @brief Holds a mapping of a (remotePane,localPoint) pair to the index in the shared node list */

			/**
			 * @brief Returns the index to the shared node list of the local point that is shared with the given remote pane.
			 * @param remotePane the remote pane id.
			 * @param localPoint the local point id.
			 * @return idx the index to the shared node list.
			 * @pre this ->hasPane( remotePane ) == true.
			 * @pre this ->hasLocalPoint( localPoint ) == true.
			 * @post this ->pconn[ remotePane ][ idx ] == localPoint.
			 */
			inline int getSharedNodeIndex( const int remotePane, const int localPoint )
			{

			  #ifdef ASSERT_ON
				  assert( this ->hasPane( remotePane ) );
				  assert( this ->hasLocalPoint( localPoint ) );
				#endif

				std::pair< int, int > keyPair = std::make_pair( remotePane, localPoint );

				#ifdef ASSERT_ON
					assert( this ->rmtpanePointPair2Index.find( keyPair ) != this ->rmtpanePointPair2Index.end( )  );
				#endif

				int idx = this ->rmtpanePointPair2Index[ keyPair ];

				#ifdef ASSERT_ON
					assert( this ->pconn[ remotePane ][ idx ] == localPoint );
				#endif

				return idx;
			}

		  /**
		   * @brief A convenience method which provides safe access to the pconn array by doing
		   * bounds checking and memory corruption checking. After, the access transaction
		   * is complete, the index is also automatically incremented.
		   * @param index the index to access.
		   * @param p the pcon array.
		   * @param size the size of the pcon array.
		   * @return p[ index ] the datum at the given index
		   * @pre index >= 0 && index < size.
		   * @pre p != NULL.
		   * @post index == index+1.
		   */
			inline int accesspcon( int &index, const int *p, const int size )
			{

				#ifdef ASSERT_ON
					assert( index >= 0 && index < size );
					assert( p != NULL );
				#endif

				int datum = p[ index ]; ++index;
				return datum;
			}

			/**
			 * @brief Builds the pane connectivity data-structured from a flat integer array returned from RocMap.
			 * @param pconnarray the pconn array returned from Rocmap.
			 * @param size the size of the pconn array.
			 */
			inline void buildPaneConnectivity( const int *pconnarray, const int size )
			{
				#ifdef ASSERT_ON
					assert( pconnarray != NULL );
					assert( size >= 0 );
				#endif

				int idx									= 0;
				int numberOfConnections = this ->accesspcon( idx, pconnarray, size );

				for( int connection=0; connection < numberOfConnections; ++connection )
				{
					int rmtpane  = this ->accesspcon( idx, pconnarray, size );
					int numnodes = this ->accesspcon( idx, pconnarray, size );

					std::vector< int > sharedNodeList;
					sharedNodeList.resize( numnodes 	);

					for( int node = 0; node < numnodes; ++node )
					{
						sharedNodeList[ node ] 			  					 = this ->accesspcon( idx, pconnarray, size )-1; /* Note, pconn is 1-based, not 0-based so 1 is subtracted */
						std::pair< int, int > keyPair 					 = std::make_pair(rmtpane, sharedNodeList[ node ] );
						this ->rmtpanePointPair2Index[ keyPair ] = node;
						this ->point2remotePaneList[ sharedNodeList[ node ] ].push_back( rmtpane );
					}

					this ->pconn[ rmtpane ] = sharedNodeList;

				}

				#ifdef ASSERT_ON
					assert( this ->pconn.size( ) == pconnarray[ 0 ] );
				#endif

			}

		public:

			/**
			 * @brief Default Constructor
			 * @post this ->getNumberOfConnection( ) == 0.
			 */
			PaneConnectivity( ){ }

			/**
			 * @brief Custom constructor. Builds a PaneConnectivity
			 * data-structured from the pconnarray returned from Roccom.
			 * @param pconnarray the pcon connectivity array.
			 * @param size the size of the pconnarray.
			 * @pre pconnarray != NULL
			 * @post this ->getNumberOfConnections( ) == pconnarray[ 0 ].
			 */
			PaneConnectivity( const int *pconnarray, const int size )
			{
				this ->buildPaneConnectivity( pconnarray, size );
			}

			/**
			 * @brief Destructor.
			 */
			~PaneConnectivity( ) { }

			/**
			 * @brief This method returns the point connection pairs for the given local point.
			 * @param localPointId the point id in query.
			 * @param connections the list of connection pairs.
			 * @note A connection pair consists of the remotePaneId and sharedNodeIndex to the
			 */
			 void getPointConnectionPairs( const int localPointId, std::vector< std::pair< int, int > > &connections )
			 {

				std::vector< int > rmtpanes;
				this ->getRemotePaneIdsSharedWithPoint( localPointId, rmtpanes );


				for( int i=0; i < rmtpanes.size( ); ++i )
				{
//					std::cout << rmtpanes[ i ] << " " << localPointId << std::endl;

					std::pair< int, int > valuePair = std::make_pair( rmtpanes[ i ], this ->getSharedNodeIndex( rmtpanes[ i ], localPointId ) );

//					std::cout << valuePair.first << " " << valuePair.second << std::endl;
					connections.push_back( valuePair );
				}

			 }

			 /**
			  * @brief Constructs a PaneConnectivity object from the pcon array returned from Rocmap.
			  * @param pconnarray the pcon connectivity array.
			  * @param size the size of the pconnarray.
			  * @pre pconnarray != NULL
			  * @post this ->getNumberOfConnections( ) == pconnarray[ 0 ].
				*/
			 void constructPaneConnectivityFromArray( const int *pconnarray, const int size )
			 {
				this ->buildPaneConnectivity( pconnarray, size );
			 }

			 /**
			  * @brief Returns the number of connections for this instance.
			  * @return N the number of inter-zone connections.
			  * @post N >= 0.
			  */
			 inline int getNumberOfConnections( ) const { return this ->pconn.size( ); }

			 /**
			  * @brief Checks if the pane exists in this instance.
			  * @param paneId the (remote) paneId to check.
			  * @return Status true if the pane exists, else false.
			  */
			 inline bool hasPane( const int paneId ) const { return( this ->pconn.find( paneId ) != this ->pconn.end( ) ); }

			 /**
			  * @brief Returns the number of nodes shared with a particular remote pane.
			  * @param remotePane the remote pane in query.
			  * @return N the number of nodes share with the remote pane.
			  * @pre this ->hasPane( remotePane ) == true.
			  * @post N >= 0.
			  */
			 int getNumberOfNodesSharedWithRemotePane( const int remotePane )
			 {
					#ifdef ASSERT_ON
					 assert( this ->hasPane( remotePane ) );
					#endif
					return( this ->pconn[ remotePane ].size( ) );
			 }

			 /**
			  * @brief This method checks if the local point exists in this instance.
			  * @param localPointId the local point id.
			  * @return Status true if the local point exists, else false.
			  * @pre localPointId >= 0.
			  */
			 inline bool hasLocalPoint( const int localPointId ) const
			 {
					#ifdef ASSERT_ON
					 assert( localPointId >= 0 );
					#endif

					if( this ->point2remotePaneList.find( localPointId ) != this ->point2remotePaneList.end( ) )
						return true;
					else
						return false;
			 }

			 /**
			  * @brief This method returns the list of remote panes that share the point associated with the provided localPointId.
			  * @param localPointId the local point id.
			  * @param rmtpaneIds the list of remote pane ids.
			  * @pre this ->hasLocalPoint( locaPointId ) == true.
			  */
			 void getRemotePaneIdsSharedWithPoint( const int localPointId, std::vector< int > &rmtpaneIds )
			 {
					#ifdef ASSERT_ON
					 assert( this ->hasLocalPoint( localPointId ) );
					#endif

					 rmtpaneIds.resize( this ->point2remotePaneList[ localPointId ].size( ) );
					 std::copy( this ->point2remotePaneList[ localPointId ].begin( ),
											this ->point2remotePaneList[ localPointId ].end( ),
											rmtpaneIds.begin( ) );
			 }

			 /**
			  * @brief Returns the remote pane ids that are connected to this instance.
			  * @param rmtpanes vector where the remote pane ids will be stored.
			  * @post rmtpanes.size( ) == this ->getNumberOfConnections( );
			  * @post this ->hasPane( rmtpanes[ i ] ) == true, for 0 <= i <= rmtpances.size( ).
			  */
			 void getRemotePaneIds( std::vector< int > &rmtpanes )
			 {
				 if( rmtpanes.size( ) != this ->getNumberOfConnections( )  )
					 rmtpanes.resize( this ->getNumberOfConnections( ) );

				 std::map< int, std::vector< int > >::iterator iter = this ->pconn.begin( );
				 for( int idx=0; iter != this ->pconn.end( ); ++iter, ++idx )
				 	 rmtpanes[ idx ] = iter ->first;
			 }

			 /**
			  * @brief This method returns the local shared point Id of the given pane at the given index.
			  * @param paneId the pane id in query.
			  * @param nodeIndex the index of the point in query.
			  * @return localID the local mesh point id of given pane.
			  * @pre this ->hasPane( paneId ) == true.
			  * @pre nodeIndex >= 0 && nodeIndex < this ->pconn[ paneId ].size( ).
			  * @post localID >= 0 && localID < N where N is the number of nodes in the mesh at the given pane.
			  */
			 int getSharedPointIdAt( const int paneId, const int nodeIndex )
			 {
					#ifdef ASSERT_ON
					 assert( this ->hasPane( paneId ) );
					 assert( nodeIndex >= 0 && nodeIndex < this ->pconn[ paneId ].size( )  );
					#endif
					return this ->pconn[ paneId ][ nodeIndex ];
			 }

			 /**
			  * @brief Returns a reference to the list of nodes shared with the remotePane.
			  * @param remotePane the id of the remote pane that has shared nodes with this instance.
			  * @return NodeList a reference to list of shared nodes.
			  * @note NodeList cannot be changed by the client of this method.
			  * @pre this ->hasPane( remotePane ) == true.
			  * @post NodeList.size( ) == this ->getNumberOfNodesSharedWithRemotePane( remotePane ).
			  */
			 std::vector< int >& getNodesSharedWithRemotePane( const int remotePane )
			 {
				#ifdef ASSERT_ON
					 assert( this ->hasPane( remotePane ) );
				#endif
				return( this ->pconn[ remotePane ] );
			 }

	}; /* End class Pane Connectivity */

} } }
#endif /* PANECONNECTIVITY_HPP_ */
