pragma solidity ^0.5.1;

contract Logger {

     // Public key of the creator of the contract.
     address creator;

     // #of block hashes written
     uint256 hash_num;

     // a block hash contains:
     // 1) the address of the writer
     // 2) the hash of the block
     // 3) the time interval corresponding to
     //    the IOT packets. 
     // Time is measured in seconds since the UNIX epoch.
     struct BlockHash {
          address writer;
          string  hash;
          uint256 start;
          uint256 end;
     }

     // logs
     mapping (uint256 => BlockHash) public logmap;
     
     // Public key of the HAB erc20 (TODO)
     // address HAB_erc20;

     event BlockWritten(string);
     
     constructor() public {
          creator  = msg.sender;
          hash_num = 0;
     }

     function log(string memory hash, uint256 start, uint256 end) public {
          require(start <= end);

          // TODO: make writing conditional on some HAB token being transferred
          // from msg.sender to SmartHAB via the HAB_erc20 contract.
          
          logmap[hash_num] = BlockHash(msg.sender, hash, start, end);
          hash_num++;
          
          emit BlockWritten(hash);
     }

}
