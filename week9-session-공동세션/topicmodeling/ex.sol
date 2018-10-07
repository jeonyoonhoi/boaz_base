pragma solidity ^0.4.11;

//msg.sender ::
//msg : 컨트랙트를 처음 생성한 것의 글로벌 주소를 저장함 

contract CarDealing {

    uint carId;
    address public buyer;
    address public seller;
    address private escrow;
    uint private start;
    bool buyerOk;       // 구매자 승인
    bool sellerOk;      // 판매자 승인

    string contractPdf;   // pdf 파일명


    //connstuctor 처음에 컨트랙트 부를 때 계약이 어떤 차에 대해 계약할건지 등 기본 적 정보를 넣고, constructor 생성자
    //
    constructor (uint car_Id, address buyer_address, address seller_address) public {
        carId = car_Id;
        buyer = buyer_address;
        seller = seller_address;
        //여기서의 msg.sender는 우리(개발프로그램...?)가 중개인이 되어주겠음
        escrow = msg.sender;    //이 솔리디티가 원래 갖고 이는 msg.sender  escrow  중개인이 되겠다
        start = now;
    }


    // accept() 라는 함순데, 만약 msg.sender가 buyer라면
    //
    function accept() public {
        if (msg.sender == buyer){
            buyerOk = true;
        } else if (msg.sender == seller){
            sellerOk = true;
        }
    }

    function setcontract(string contract_Pdf) private {
         if (buyerOk && sellerOk){
             contractPdf = contract_Pdf;
         }
    }

    function kill() public constant {
        if (msg.sender == escrow) {
            selfdestruct(buyer);
        }
    }
}
