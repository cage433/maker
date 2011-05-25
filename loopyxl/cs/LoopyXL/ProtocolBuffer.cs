using loopyxl;

namespace LoopyXL
{
    public interface ProtocolBuffer
    {
        Response Call(Request request);
    }
}