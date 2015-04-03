/**
 *  @brief The MeshPointIdToGlobalIdMap provides the functionality for mapping
 *   a pair consisting of (mesh_pane_id, mesh_point_id) to mesh_global_id.
 *  @file MeshPointIdToGlobalIdMap.hpp
 *  @date Aug 14, 2009
 *  @author George Zagaris (gzagaris@illinois.edu)
 */

#ifndef MESHPOINTIDTOGLOBALIDMAP_HPP_
#define MESHPOINTIDTOGLOBALIDMAP_HPP_

#include <map>
#include <utility>

namespace Rocstar { namespace Rocin { namespace Utilities {



	class MeshPointIdToGlobalIdMap
	{
		private:
			std::map< std::pair< int, int >, int > local2global;

		public:

			/**
			 * @brief Constructor.
			 * @post this ->size( ) == 0.
			 */
			MeshPointIdToGlobalIdMap( ) { }

			/**
			 * @brief Destructor.
			 */
			~MeshPointIdToGlobalIdMap( ){ this ->local2global.clear( ); }

			/**
			 * @brief Checks if the point meshPointId of the mesh associated with the
			 * provided meshPaneId exists in this MeshPointIdToGlobalIdMap instance.
			 * @param meshPaneId the pane id of the mesh in query.
			 * @param meshPointId the local point id of the point in query.
			 * @return Status true iff exists else false.
			 */
			inline bool exists( const int meshPaneId, const int meshPointId )
			{
				std::pair< int, int > keyPair = std::make_pair( meshPaneId, meshPointId );
				if( this ->local2global.find( keyPair ) != this ->local2global.end( ) )
					return true;
				else
					return false;
			}

			/**
			 * @brief Returns the assigned global id to meshPointId of the mesh
			 * associated with the provided meshPaneId.
			 * @param meshPaneId the pane id of the mesh in query.
			 * @param meshPointId the point id of the mesh point in query.
			 * @return ID the global id assigned to the point.
			 * @post ID >= 0 or -1 iff ! this ->exists( meshPaneId, meshPoint ).
			 */
			inline int getGlobalId( const int meshPaneId, const int meshPointId )
			{
				std::pair< int, int > keyPair = std::make_pair( meshPaneId, meshPointId );
				if( this ->local2global.find( keyPair ) != this ->local2global.end( ) )
					return( this ->local2global[ keyPair ] );
				else
					return(-1);
			}

			/**
			 * @brief This method constructs a new key-pair for the meshPaneId and meshPointId
			 * which maps into globalId and inserts it into the data-structure.
			 * @param meshPaneId the pane id of the mesh in query.
			 * @param meshPointId the point id of the mesh point in query.
			 * @param globalId the global id assigned to the point.
			 * @pre globalId >= 0 && meshPointId >= 0.
			 * @pre this ->exists( meshPaneId, meshPointId ) == false.
			 * @post this ->exists( meshPaneId, meshPointId ) == true.
			 */
			inline void insert( const int meshPaneId, const int meshPointId, const int globalId )
			{

//				std::cout << "Mesh Pane Id: " << meshPaneId << " ";
//				std::cout << "Mesh Point Id: " << meshPointId << " ";
//				std::cout << std::endl;
//				std::cout.flush( );

				#ifdef ASSERT_ON
					assert( globalId >= 0 );
					assert( meshPointId >= 0);
					assert( !this ->exists( meshPaneId, meshPointId ) );
				#endif

				std::pair< int, int > keyPair  = std::make_pair( meshPaneId, meshPointId );
				this ->local2global[ keyPair ] = globalId;

				#ifdef ASSERT_ON
					assert( this ->exists( meshPaneId, meshPointId ) );
				#endif

			}

			/**
			 * @brief Returns the size of the map.
			 * @return N the number of elements in the map.
			 * @post N >= 0.
			 */
			inline int size( ) const { return this ->local2global.size( ); }


	};

} } }

#endif /* MESHPOINTIDTOGLOBALIDMAP_HPP_ */
