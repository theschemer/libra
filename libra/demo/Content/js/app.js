$(function(){
    $.ajax({
        url: "/blog/libra/17",
        success: function(data){
            $('.name').html(data.name);
        }
    });
})